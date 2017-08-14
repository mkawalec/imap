module Network.IMAP.RequestWatcher (requestWatcher) where

import Network.IMAP.Types
import Network.IMAP.Parsers

import Data.Maybe (isJust, fromJust)

import Network.Connection
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.List as L
import qualified Data.Text as T

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent (killThread)
import Control.Monad.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (SomeException)
import qualified Control.Monad.Catch as C
import Control.Monad.Catch (MonadCatch)
import Control.Monad (when)
import Data.Foldable (forM_)

import System.Log.Logger (errorM)


requestWatcher :: (MonadIO m, Universe m, MonadCatch m) => IMAPConnection -> m ()
requestWatcher conn = flip C.catch (handleExceptions conn) $ do
  parseResult <- getParsedChunk (rawConnection . imapState $ conn) (AP.parse parseReply)
  reactToReply conn parseResult

  requestWatcher conn

reactToReply :: (MonadIO m) =>
  IMAPConnection ->
  ParseResult ->
  m ()
reactToReply conn parsedReply = do
  let state = imapState conn
  requests <- liftIO . atomically $ do
    newReqs <- getOutstandingReqs $ responseRequests state
    knownReqs <- readTVar $ outstandingReqs state
    return $ knownReqs ++ newReqs

  pendingReqs <- case parsedReply of
    Left err -> dispatchError requests err
    Right reply -> do
      updateConnState conn reply
      case reply of
        Tagged t -> dispatchTagged requests t
        Untagged u -> dispatchUntagged conn requests u

  liftIO . atomically $ writeTVar (outstandingReqs state) pendingReqs
  shouldIDie conn

updateConnState :: (MonadIO m) => IMAPConnection -> CommandResult -> m ()
updateConnState conn command = do
  let connState = connectionState conn

  case command of
    Untagged u -> case u of
                    OKResult _ -> liftIO . atomically $ writeTVar connState Connected
                    Bye -> liftIO . atomically $ writeTVar connState Disconnected
                    _ -> return ()
    _ -> return ()

shouldIDie :: (MonadIO m) => IMAPConnection -> m ()
shouldIDie conn = liftIO $ do
  threadId <- atomically . readTVar . serverWatcherThread . imapState $ conn
  connState <- atomically . readTVar $ connectionState conn

  when (isDisconnected connState && isJust threadId) $
    killThread $ fromJust threadId

dispatchError :: (MonadIO m) => [ResponseRequest] ->
  ErrorMessage -> m [ResponseRequest]
dispatchError requests errorMessage = do
  case requests of
    req:reqs -> do
      let errorResponse = TaggedResult {
        commandId="noid"
      , resultState=BAD
      , resultRest=errorMessage
      }
      liftIO . atomically $ writeTQueue (responseQueue req) $ Tagged errorResponse
      return reqs
    _ -> do
      liftIO $ errorM "dispatchError" "Cannot dispatch a parse error \
        \without an outstanding request"
      return []

dispatchTagged :: (MonadIO m) => [ResponseRequest] ->
  TaggedResult -> m [ResponseRequest]
dispatchTagged requests response = do
  let reqId = commandId response
  let pendingRequest = L.find (\r -> respRequestId r == reqId) requests

  liftIO $ case pendingRequest of
    Just req -> atomically $ writeTQueue (responseQueue req) $ Tagged response
    Nothing -> return ()

  return $ if isJust pendingRequest
            then filter (/= fromJust pendingRequest) requests
            else requests

dispatchUntagged :: (MonadIO m) => IMAPConnection ->
                    [ResponseRequest] ->
                    UntaggedResult ->
                    m [ResponseRequest]
dispatchUntagged conn requests response = do
  if null requests
    then liftIO . atomically $ RQ.write (untaggedQueue conn) response
    else liftIO . atomically $ do
      let responseQ = responseQueue . head $ requests
      writeTQueue responseQ $ Untagged response
  return requests

getOutstandingReqs :: TQueue ResponseRequest ->
                      STM [ResponseRequest]
getOutstandingReqs reqsQueue = do
  isEmpty <- isEmptyTQueue reqsQueue
  if isEmpty
    then return []
    else do
      req <- readTQueue reqsQueue
      next <- getOutstandingReqs reqsQueue
      return (req:next)

omitOneLine :: BSC.ByteString -> BSC.ByteString
omitOneLine bytes = if BSC.length withLF > 0 then BSC.tail withLF else withLF
  where withLF = BSC.dropWhile (/= '\n') bytes

parseChunk :: (BSC.ByteString -> Result ParseResult) ->
              BSC.ByteString ->
              ((Maybe ParseResult, Maybe (BSC.ByteString -> Result ParseResult)), BSC.ByteString)
parseChunk parser chunk =
    case parser chunk of
      Fail left _ msg -> ((Just $ assembleParseError msg chunk, Nothing), omitOneLine left)
      Partial continuation -> ((Nothing, Just continuation), BS.empty)
      Done left result -> ((Just result, Nothing), left)

assembleParseError :: String -> BSC.ByteString -> ParseResult
assembleParseError parserError chunk = Left $ T.concat [
    "Parse failed with error: '", packedError, "' while reading an input chunk: '",
    decodedChunk, "'.\n\nThis should never happen and is a library error. ",
    "To open an issue please go to https://github.com/mkawalec/imap/issues"]
  where packedError = T.pack parserError
        decodedChunk = T.pack $ show chunk

getParsedChunk :: (MonadIO m, Universe m) => Connection ->
                  (BSC.ByteString -> Result ParseResult) ->
                  m ParseResult
getParsedChunk conn parser = do
  (parsed, cont) <- connectionGetChunk'' conn $ parseChunk parser

  case cont of
    Just continuation -> getParsedChunk conn continuation
    Nothing -> return . fromJust $ parsed

-- |Reject all outstanding requests with the exception handler, close the watcher
handleExceptions :: (MonadIO m) => IMAPConnection ->
                                   SomeException ->
                                   m ()
handleExceptions conn e = do
  let state = imapState conn

  threadId <- liftIO . atomically $ do
    writeTVar (connectionState conn) Disconnected
    let actualThreadId = readTVar $ serverWatcherThread state
    writeTVar (serverWatcherThread state) Nothing
    actualThreadId

  requests <- liftIO . atomically $ do
    newReqs <- getOutstandingReqs $ responseRequests state
    knownReqs <- readTVar $ outstandingReqs state
    return $ knownReqs ++ newReqs

  let reply = TaggedResult {
    commandId = "noid",
    resultState = BAD,
    resultRest = T.append "Exception caught " (T.pack . show $ e)
  }
  liftIO . atomically $ mapM_ (sendResponse reply) requests

  forM_ threadId $ liftIO . killThread

sendResponse :: TaggedResult -> ResponseRequest -> STM ()
sendResponse response request = writeTQueue (responseQueue request) $ Tagged response
