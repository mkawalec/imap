-----------------------------------------------------------------------------
-- |
-- Module      :  Network.IMAP
-- Copyright   :  2016 Michal Kawalec
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Michal Kawalec <michal@monad.cat>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Usage:
--
-- @
--    import Network.Connection
--    import Network.IMAP
--
--    let tls = TLSSettingsSimple False False False
--    let params = ConnectionParams "imap.gmail.com" 993 (Just tls) Nothing
--    conn <- connectServer params
--    simpleFormat $ login conn "mylogin" "mypass"
-- @
--
-- For more usage examples, please see the readme
module Network.IMAP (
  connectServer,
  sendCommand,
  startTLS,
  capability,
  noop,
  logout,
  login,
  authenticate,
  select,
  examine,
  create,
  delete,
  rename,
  subscribe,
  unsubscribe,
  list,
  lsub,
  status,
  append,
  Network.IMAP.check,
  close,
  expunge,
  search,
  uidSearch,
  fetch,
  uidFetch,
  fetchG,
  uidFetchG,
  store,
  uidStore,
  copy,
  uidCopy,
  simpleFormat
) where

import Network.Connection
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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

import Control.Monad (MonadPlus(..), when)
import Control.Monad.IO.Class (MonadIO(..))
import ListT (toList, ListT)
import qualified Data.List as L

-- |Connects to the server and gives you a connection object
--  that needs to be passed to any other command. You should only call it once
--  for every connection you wish to create
connectServer :: ConnectionParams -> Maybe IMAPSettings -> IO IMAPConnection
connectServer connParams wrappedSettings = do
  context <- initConnectionContext
  connection <- connectTo context connParams
  let settings = if isJust wrappedSettings then fromJust wrappedSettings else defaultImapSettings

  untaggedRespsQueue <- RQ.newIO $ untaggedQueueLength settings
  responseRequestsQueue <- newTQueueIO
  connState <- newTVarIO UndefinedState
  watcherId <- newTVarIO Nothing
  requests <- newTVarIO []

  let state = IMAPState {
    rawConnection = connection,
    connectionContext = context,
    responseRequests = responseRequestsQueue,
    serverWatcherThread = watcherId,
    outstandingReqs = requests,
    imapSettings = settings
  }

  let conn = IMAPConnection {
    connectionState = connState,
    untaggedQueue = untaggedRespsQueue,
    imapState = state
  }

  watcherThreadId <- forkIO $ requestWatcher conn
  atomically $ writeTVar (serverWatcherThread . imapState $ conn)
    (Just watcherThreadId)

  return conn

-- |An escape hatch, gives you the ability to send any command to the server,
--  even one not implemented by this library
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
  readResults state responseRequest

-- |
-- = Connected state commands

-- |Upgrade a connection to a TLS connection from an insecure one. Accepts TLS settings
--  you wish your connection to use
startTLS :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  TLSSettings -> m CommandResult
startTLS conn tls = do
  res <- sendCommand conn "STARTTLS"
  let state = imapState conn

  case res of
    Tagged (TaggedResult _ resState _) -> when (resState == OK) $
      do
        threadId <- liftIO . atomically . readTVar $ serverWatcherThread state
        liftIO . killThread . fromJust $ threadId
        liftIO $ connectionSetSecure (connectionContext state) (rawConnection state) tls

        watcherThreadId <- liftIO . forkIO $ requestWatcher conn
        liftIO . atomically $ do
          writeTVar (serverWatcherThread state) $ Just watcherThreadId
          writeTVar (connectionState conn) Connected
    _ -> return ()

  return res

capability :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
capability conn = sendCommand conn "CAPABILITY"

noop :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
noop conn = sendCommand conn "NOOP"

logout :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
logout conn = sendCommand conn "LOGOUT"

-- |A simple authentication method, with user and password.
--  Probably what's needed in 90% of cases.
login :: (MonadPlus m, MonadIO m, Universe m) =>
         IMAPConnection ->
         T.Text ->
         T.Text ->
         m CommandResult
login conn username password = sendCommand conn . encodeUtf8 $
  T.intercalate " " ["LOGIN", escapeText username, escapeText password]

-- |Authenticate with the server. During the authentication control is given
--  to the library user and is returned to the library at the end of authentication
authenticate :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  BSC.ByteString -> (IMAPConnection -> m ()) -> m ()
authenticate conn method authAction = do
  requestId <- liftIO genRequestId
  let state = imapState conn
  let commandLine = BSC.concat [requestId, " AUTHENTICATE ", method, "\r\n"]

  connectionPut' (rawConnection . imapState $ conn) commandLine

  -- kill the watcher thread
  threadId <- liftIO . atomically . readTVar . serverWatcherThread $ state
  liftIO . killThread . fromJust $ threadId

  authAction conn

  -- Bring the watcher back up
  watcherThreadId <- liftIO . forkIO $ requestWatcher conn
  liftIO . atomically $ do
    writeTVar (serverWatcherThread state) $ Just watcherThreadId
    writeTVar (connectionState conn) Connected

  return ()

-- |
-- = Authenticated state commands


select :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
select conn mboxName = oneParamCommand "SELECT" conn escapedMailbox
  where escapedMailbox = T.concat ["\"", mboxName, "\""]

examine :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
examine conn mboxName = oneParamCommand "EXAMINE" conn escapedMailbox
  where escapedMailbox = T.concat ["\"", mboxName, "\""]

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

append :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> BSC.ByteString -> Maybe [Flag] -> Maybe T.Text -> m CommandResult
append conn mailboxName message flagL dateTime = do
  let encodedFlags = if isJust flagL
                      then BSC.concat [" ", flagsToText $ fromJust flagL]
                      else BSC.empty
  let encodedDate = if isJust dateTime
                      then BSC.concat [" \"", encodeUtf8 . fromJust $ dateTime, "\""]
                      else BSC.empty

  let command = BSC.concat ["APPEND ", encodeUtf8 mailboxName, encodedFlags,
                            encodedDate, " {", BSC.pack . show . BSC.length $ message,
                            "}\r\n", message]

  return ()
  sendCommand conn command

-- |
-- = Selected state commands
check :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
check conn = sendCommand conn "CHECK"

close :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
close conn = sendCommand conn "CLOSE"

expunge :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> m CommandResult
expunge conn = sendCommand conn "EXPUNGE"

search :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> T.Text ->
  m CommandResult
search = oneParamCommand "SEARCH"

uidSearch :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection -> T.Text ->
  m CommandResult
uidSearch = oneParamCommand "UID SEARCH"

-- |Fetch message body by message sequence id
fetch :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
fetch conn query = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["FETCH", query, "BODY[]"]

-- |Fetch message body by message UID
uidFetch :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
uidFetch conn query = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["UID FETCH", query, "BODY[]"]

-- |A general fetch, you have to specify everything that
--  goes after the `FETCH` keyword
fetchG :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
fetchG = oneParamCommand "FETCH"

-- |A general fetch using UIDs
uidFetchG :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> m CommandResult
uidFetchG = oneParamCommand "UID FETCH"


store :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> [Flag] -> m CommandResult
store conn sequenceSet dataItem flagList = do
  let command = BSC.intercalate " " ["STORE", encodeUtf8 sequenceSet,
                                     encodeUtf8 dataItem, flagsToText flagList]
  sendCommand conn command

uidStore :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> [Flag] -> m CommandResult
uidStore conn sequenceSet dataItem flagList = do
  let command = BSC.intercalate " " ["UID STORE", encodeUtf8 sequenceSet,
                                     encodeUtf8 dataItem, flagsToText flagList]
  sendCommand conn command


copy :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> m CommandResult
copy conn sequenceSet mailboxName = sendCommand conn command
  where command = BSC.intercalate " " ["COPY", encodeUtf8 sequenceSet,
                                       encodeUtf8 mailboxName]

-- |Copy message by message UID
uidCopy :: (MonadPlus m, MonadIO m, Universe m) => IMAPConnection ->
  T.Text -> T.Text -> m CommandResult
uidCopy conn sequenceSet mailboxName = sendCommand conn $ encodeUtf8 command
  where command = T.intercalate " " ["UID COPY", sequenceSet, mailboxName]


-- |Return the untagged replies or an error message if the tagged reply
--  is of type NO or BAD. Also return all untagged replies received if
--  replies list contains a BYE response
--  (when the server decided to cleanly disconnect)
simpleFormat :: (MonadIO m) =>
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
            _ -> return . Left . resultRest $ t

oneParamCommand :: (MonadPlus m, MonadIO m, Universe m) => T.Text ->
  IMAPConnection -> T.Text -> m CommandResult
oneParamCommand commandName conn params = sendCommand conn wholeCommand
  where wholeCommand = encodeUtf8 $ T.intercalate " " [commandName, params]
