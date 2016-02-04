module Network.IMAP (
  connectServer,
  sendCommand,
  login,
  logout,
  capability,
  select,
  noop,
  simpleFormat
) where

import Network.Connection
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BSC

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Control.Concurrent (forkIO)

import Network.IMAP.Types
import Network.IMAP.RequestWatcher
import Network.IMAP.Utils

import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import ListT (toList, ListT)

connectServer :: IO IMAPConnection
connectServer = do
  context <- initConnectionContext
  let params = ConnectionParams "imap.gmail.com" 993 Nothing Nothing
  let tlsSettings = TLSSettingsSimple False False False

  connection <- connectTo context params
  connectionSetSecure context connection tlsSettings

  untaggedRespsQueue <- RQ.newIO 20
  responseRequestsQueue <- newTQueueIO
  connState <- newTVarIO UndefinedState
  watcherId <- newTVarIO Nothing

  let state = IMAPState {
    rawConnection = connection,
    responseRequests = responseRequestsQueue
  }

  let conn = IMAPConnection {
    connectionState = connState,
    serverWatcherThread = watcherId,
    untaggedQueue = untaggedRespsQueue,
    imapState = state
  }

  watcherThreadId <- forkIO $ requestWatcher conn []
  atomically $ writeTVar watcherId $ Just watcherThreadId

  return conn

sendCommand :: (MonadPlus m, MonadIO m, Universe m) =>
               IMAPConnection ->
               BSC.ByteString ->
               m CommandResult
sendCommand conn command = ifNotDisconnected conn $ do
  let state = imapState conn
  requestId <- liftIO genRequestId
  responseQ <- liftIO . atomically $ newTQueue
  let commandLine = BSC.concat [requestId, " ", command, "\r\n"]

  let responseRequest = ResponseRequest responseQ requestId
  liftIO . atomically $ writeTQueue (responseRequests state) responseRequest

  connectionPut' (rawConnection state) commandLine
  readResults responseQ

login :: (MonadPlus m, MonadIO m, Universe m) =>
         IMAPConnection ->
         T.Text ->
         T.Text ->
         m CommandResult
login conn username password = sendCommand conn . encodeUtf8 $
  T.intercalate " " ["LOGIN", escapeText username, escapeText password]

capability :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
capability conn = sendCommand conn "CAPABILITY"

noop :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
noop conn = sendCommand conn "NOOP"

logout :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
logout conn = sendCommand conn "LOGOUT"

select :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> T.Text -> m CommandResult
select conn mailboxName = sendCommand conn command
  where command = encodeUtf8 $ T.append "SELECT " mailboxName

examine :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> T.Text -> m CommandResult
examine conn mailboxName = sendCommand conn command
  where command = encodeUtf8 $ T.append "EXAMINE " mailboxName

simpleFormat :: (MonadIO o, Universe o) =>
                ListT o CommandResult -> o SimpleResult
simpleFormat action = do
  results <- toList action
  case last results of
    Untagged _ -> return . Left $ "Last result is untagged, something went wrong"
    Tagged t -> case resultState t of
      OK -> return . Right $ map (\(Untagged u) -> u) (init results)
      _ -> return . Left $ T.concat ["Error '", decodeUtf8 . resultRest $ t, "'"]
