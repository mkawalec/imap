module Network.IMAP where

import Network.Connection
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BSC

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Control.Concurrent (forkIO)

import Network.IMAP.Types
import Network.IMAP.RequestWatcher
import Network.IMAP.Utils

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (MonadTrans(..))


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

sendCommand :: (MonadTrans t, MonadPlus (t IO)) =>
               IMAPConnection ->
               BSC.ByteString ->
               t IO CommandResult
sendCommand conn command = do
  let state = imapState conn
  requestId <- lift genRequestId
  let commandLine = BSC.concat [requestId, " ", command, "\r\n"]

  lift $ connectionPut (rawConnection state) commandLine
  responseQ <- lift . atomically $ newTQueue

  let responseRequest = ResponseRequest responseQ requestId
  lift . atomically $ writeTQueue (responseRequests state) responseRequest
  readResults responseQ

readResults :: (MonadTrans t, MonadPlus (t IO)) =>
               TQueue CommandResult ->
               t IO CommandResult
readResults resultsQueue = do
  nextResult <- lift . atomically . readTQueue $ resultsQueue
  case nextResult of
    Tagged _ -> return nextResult
    Untagged _ -> (return nextResult) `mplus` readResults resultsQueue

login :: (MonadTrans t, MonadPlus (t IO)) =>
         IMAPConnection ->
         T.Text ->
         T.Text ->
         t IO CommandResult
login conn username password = sendCommand conn . encodeUtf8 $
  T.intercalate " " ["LOGIN", escapeText username, escapeText password]

escapeText :: T.Text -> T.Text
escapeText t = T.replace "{" "\\{" $
             T.replace "}" "\\}" $
             T.replace "\"" "\\\"" $
             T.replace "\\" "\\\\" t
