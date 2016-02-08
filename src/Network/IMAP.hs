module Network.IMAP (
  connectServer,
  sendCommand,
  authenticate,
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
  startTLS,
  uidFetchG,
  append,
  store,
  copy
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

import Control.Concurrent (forkIO, killThread)

import Network.IMAP.Types
import Network.IMAP.RequestWatcher
import Network.IMAP.Utils

import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import ListT (toList, ListT)
import Data.Either.Combinators (isRight)
import qualified Data.List as L
import qualified Debug.Trace as DT

connectServer :: ConnectionParams -> IO IMAPConnection
connectServer connParams = do
  context <- initConnectionContext
  connection <- connectTo context connParams

  untaggedRespsQueue <- RQ.newIO 20
  responseRequestsQueue <- newTQueueIO
  connState <- newTVarIO UndefinedState
  watcherId <- newTVarIO Nothing
  requests <- newTVarIO []

  let state = IMAPState {
    rawConnection = connection,
    connectionContext = context,
    responseRequests = responseRequestsQueue,
    outstandingReqs = requests
  }

  let conn = IMAPConnection {
    connectionState = connState,
    serverWatcherThread = watcherId,
    untaggedQueue = untaggedRespsQueue,
    imapState = state
  }

  watcherThreadId <- forkIO $ requestWatcher conn
  atomically $ writeTVar (serverWatcherThread conn) $ Just watcherThreadId

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

startTLS :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
startTLS conn = do
  res <- sendCommand conn "STARTTLS"

  case res of
    Tagged (TaggedResult _ resState _) -> if resState == OK
      then do
        threadId <- liftIO . atomically . readTVar $ serverWatcherThread conn
        liftIO . killThread . fromJust $ threadId
        let tls = TLSSettingsSimple False False False
        let state = imapState conn
        liftIO $ connectionSetSecure (connectionContext state) (rawConnection state) tls

        watcherThreadId <- liftIO . forkIO $ requestWatcher conn
        liftIO . atomically $ do
          writeTVar (serverWatcherThread conn) $ Just watcherThreadId
          writeTVar (connectionState conn) $ Connected
      else return ()
    _ -> return ()

  return res

-- |Authenticate with the server. During the authentication control is given
--  to the library user and is returned to the library at the end of authentication
authenticate :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  BSC.ByteString -> (IMAPConnection -> m ()) -> m ()
authenticate conn method authAction = do
  requestId <- liftIO genRequestId
  let commandLine = BSC.concat [requestId, " AUTHENTICATE ", method, "\r\n"]

  connectionPut' (rawConnection . imapState $ conn) commandLine

  -- kill the watcher thread
  threadId <- liftIO . atomically . readTVar $ serverWatcherThread conn
  liftIO . killThread . fromJust $ threadId

  authAction conn

  -- Bring the watcher back up
  watcherThreadId <- liftIO . forkIO $ requestWatcher conn
  liftIO . atomically $ do
    writeTVar (serverWatcherThread conn) $ Just watcherThreadId
    writeTVar (connectionState conn) $ Connected

  return ()

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
list conn mailboxName = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " ["LIST", "\"\"", mailboxName]

lsub :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
lsub conn mailboxName = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " ["LSUB", "\"\"", mailboxName]

status :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
status conn mailboxName = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["STATUS", mailboxName,
                                     "(MESSAGES", "RECENT", "UIDNEXT",
                                     "UIDVALIDITY", "UNSEEN)"]

check :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
check conn = sendCommand conn "CHECK"

close :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
close conn = sendCommand conn "CLOSE"

expunge :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
expunge conn = sendCommand conn "EXPUNGE"

generalSearch :: T.Text -> IMAPConnection ->
  T.Text -> IO (Either ErrorMessage [Int])
generalSearch commandName conn query = do
  result <- simpleFormat (oneParamCommand commandName conn query :: ListT IO CommandResult)

  case result of
    Right u -> case head u of
      Search res -> return . Right $ res
      _ -> return . Left $ "Received reply of a wrong type"
    Left l -> return . Left $ l

search :: IMAPConnection -> T.Text -> IO (Either ErrorMessage [Int])
search = generalSearch "SEARCH"

uidSearch :: IMAPConnection -> T.Text -> IO (Either ErrorMessage [Int])
uidSearch = generalSearch "UID SEARCH"


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

append :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> BSC.ByteString -> Maybe [Flag] -> Maybe T.Text -> m CommandResult
append conn mailboxName message flags dateTime = do
  let encodedFlags = if isJust flags
                      then BSC.concat [" ", flagsToText $ fromJust flags]
                      else BSC.empty
  let encodedDate = if isJust dateTime
                      then BSC.concat [" \"", encodeUtf8 . fromJust $ dateTime, "\""]
                      else BSC.empty

  let command = BSC.concat ["APPEND ", encodeUtf8 mailboxName, encodedFlags,
                            encodedDate, " {", BSC.pack . show . BSC.length $ message,
                            "}\r\n", message]

  DT.traceShow command $ return ()
  sendCommand conn command

store :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> [Flag] -> m CommandResult
store conn sequenceSet dataItem flagList = do
  let command = BSC.intercalate " " ["STORE", encodeUtf8 sequenceSet,
                                     encodeUtf8 dataItem, flagsToText flagList]
  sendCommand conn command


copy :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> m CommandResult
copy conn sequenceSet mailboxName = sendCommand conn command
  where command = BSC.intercalate " " ["COPY", encodeUtf8 sequenceSet,
                                       encodeUtf8 mailboxName]


-- |Return the untagged replies or an error message if the tagged reply
--  is of type NO or BAD. Also return all untagged replies received if
--  replies list contains a BYE response
--  (when the server decided to cleanly disconnect)
simpleFormat :: (MonadIO m, Universe m) =>
                ListT m CommandResult -> m SimpleResult
simpleFormat action = do
  results <- toList action
  let
    hasBye = L.find (\i -> case i of
      Untagged u -> isBye u
      Tagged _ -> False) results

  if isJust hasBye
    then return . Right $ map (\(Untagged u) -> u) $ filter isUntagged results
    else case last results of
          Untagged _ -> return . Left $ "Last result is untagged, something went wrong"
          Tagged t -> case resultState t of
            OK -> return . Right $ map (\(Untagged u) -> u) (init results)
            _ -> return . Left . decodeUtf8 . resultRest $ t
