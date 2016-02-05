module Network.IMAP (
  connectServer,
  sendCommand,
  login,
  logout,
  capability,
  select,
  examine,
  create,
  delete,
  rename,
  noop,
  subscribe,
  unsubscribe,
  list,
  lsub,
  status,
  Network.IMAP.check,
  close,
  expunge,
  simpleFormat,
  search,
  uidSearch,
  fetch,
  uidFetch,
  fetchG,
  uidFetchG,
) where

import Network.Connection
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BSC

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Maybe (isJust, fromJust)

import Control.Concurrent (forkIO)

import Network.IMAP.Types
import Network.IMAP.RequestWatcher
import Network.IMAP.Utils

import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import ListT (toList, ListT)

connectServer :: ConnectionParams -> Maybe TLSSettings -> IO IMAPConnection
connectServer connParams tlsSettings = do
  context <- initConnectionContext
  connection <- connectTo context connParams

  if isJust tlsSettings
    then connectionSetSecure context connection $ fromJust tlsSettings
    else return ()

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

oneParamCommand :: (MonadPlus m, MonadIO m, Universe m) => T.Text ->
  IMAPConnection -> T.Text -> m CommandResult
oneParamCommand commandName conn mailboxName = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " [commandName, mailboxName]

select :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
select = oneParamCommand "SELECT"

examine :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
examine = oneParamCommand "EXAMINE"

create :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
create = oneParamCommand "CREATE"

delete :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
delete = oneParamCommand "DELETE"

rename :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> m CommandResult
rename conn fromName toName = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " ["RENAME", fromName, toName]

subscribe :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
subscribe = oneParamCommand "SUBSCRIBE"

unsubscribe :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
unsubscribe = oneParamCommand "UNSUBSCRIBE"

list :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
list conn inboxName = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " ["LIST", "\"\"", inboxName]

lsub :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
lsub conn inboxName = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " ["LSUB", "\"\"", inboxName]

status :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
status conn inboxName = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["STATUS", inboxName, "(MESSAGES", "RECENT", "UIDNEXT", "UIDVALIDITY", "UNSEEN)"]

check :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
check conn = sendCommand conn "CHECK"

close :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
close conn = sendCommand conn "CLOSE"

expunge :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
expunge conn = sendCommand conn "EXPUNGE"

search :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
search = oneParamCommand "SEARCH"

uidSearch :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
uidSearch = oneParamCommand "UID SEARCH"

fetch :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
fetch conn query = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["FETCH", query, "BODY[]"]

uidFetch :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
uidFetch conn query = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["UID FETCH", query, "BODY[]"]

fetchG :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
fetchG = oneParamCommand "FETCH"

uidFetchG :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
uidFetchG = oneParamCommand "UID FETCH"

simpleFormat :: (MonadIO o, Universe o) =>
                ListT o CommandResult -> o SimpleResult
simpleFormat action = do
  results <- toList action
  case last results of
    Untagged _ -> return . Left $ "Last result is untagged, something went wrong"
    Tagged t -> case resultState t of
      OK -> return . Right $ map (\(Untagged u) -> u) (init results)
      _ -> return . Left $ T.concat ["Error '", decodeUtf8 . resultRest $ t, "'"]
