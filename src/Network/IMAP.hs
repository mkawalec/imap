module Network.IMAP where

import Network.Connection
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BSC

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Concurrent (forkIO)

import Network.IMAP.Types
import Network.IMAP.RequestWatcher
import Network.IMAP.Utils

import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))

newtype Bytes a = Bytes BSC.ByteString
class Monad m => OverloadableConnection m where
  bytesWritten :: TVar (Bytes (m ()))
  bytesToWrite :: TMVar (Bytes (m ()))

  connectionPut :: Connection -> BSC.ByteString -> m ()
  connectionGetChunk' :: Connection -> (BSC.ByteString -> (a, BSC.ByteString)) -> m a

instance OverloadableConnection IO where
  connectionPut = Network.Connection.connectionPut

connectServer :: IO IMAPConnection
connectServer = do
  context <- initConnectionContext
  let params = ConnectionParams "imap.gmail.com" 993 Nothing Nothing
  let tlsSettings = TLSSettingsSimple False False False

  connection <- connectTo context params
  connectionSetSecure context connection tlsSettings

  untaggedRespsQueue <- RQ.newIO 20
  responseRequestsQueue <- newTQueueIO

  let state = IMAPState {
    rawConnection = connection,
    responseRequests = responseRequestsQueue
  }

  let conn = IMAPConnection {
    connectionState = Connected,
    serverWatcherThread = Nothing,
    untaggedQueue = untaggedRespsQueue,
    imapState = state
  }

  watcherThreadId <- forkIO $ requestWatcher conn []

  return conn {
    serverWatcherThread = Just watcherThreadId
  }

sendCommand :: (MonadPlus m, MonadIO m, OverloadableConnection m) =>
               IMAPConnection ->
               BSC.ByteString ->
               m CommandResult
sendCommand conn command = do
  let state = imapState conn
  requestId <- liftIO genRequestId
  let commandLine = BSC.concat [requestId, " ", command, "\r\n"]

  liftIO $ Network.IMAP.connectionPut (rawConnection state) commandLine
  responseQ <- liftIO . atomically $ newTQueue

  let responseRequest = ResponseRequest responseQ requestId
  liftIO . atomically $ writeTQueue (responseRequests state) responseRequest
  readResults responseQ

readResults :: (MonadPlus m, MonadIO m) =>
               TQueue CommandResult ->
               m CommandResult
readResults resultsQueue = do
  nextResult <- liftIO . atomically . readTQueue $ resultsQueue
  case nextResult of
    Tagged _ -> return nextResult
    Untagged _ -> (return nextResult) `mplus` readResults resultsQueue

login :: (MonadPlus m, MonadIO m, OverloadableConnection m) =>
         IMAPConnection ->
         T.Text ->
         T.Text ->
         m CommandResult
login conn username password = sendCommand conn . encodeUtf8 $
  T.intercalate " " ["LOGIN", escapeText username, escapeText password]

escapeText :: T.Text -> T.Text
escapeText t = T.replace "{" "\\{" $
             T.replace "}" "\\}" $
             T.replace "\"" "\\\"" $
             T.replace "\\" "\\\\" t
