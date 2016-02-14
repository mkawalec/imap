module Network.IMAP.RequestWatcher (requestWatcher) where

import Network.IMAP.Types
import Network.IMAP.Parsers

import Data.Either (isRight)
import Data.Either.Combinators (fromRight')
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

import System.Log.Logger (errorM)


requestWatcher :: (MonadIO m, Universe m, MonadCatch m) => IMAPConnection -> m ()
requestWatcher conn = flip C.catch (handleExceptions conn) $ do
  parsedLine <- getParsedChunk (rawConnection . imapState $ conn) (AP.parse parseReply)

  if isRight parsedLine
    then reactToReply conn $ fromRight' parsedLine
    else return ()

  requestWatcher conn

reactToReply :: (MonadIO m, Universe m, MonadCatch m) => IMAPConnection -> CommandResult -> m ()
reactToReply conn parsedReply = do
  let state = imapState conn
  requests <- liftIO . atomically $ do
    newReqs <- getOutstandingReqs $ responseRequests state
    knownReqs <- readTVar $ outstandingReqs state
    return $ knownReqs ++ newReqs

  updateConnState conn parsedReply
  pendingReqs <- case parsedReply of
    Tagged t -> dispatchTagged requests t
    Untagged u -> dispatchUntagged conn requests u

  liftIO . atomically $ writeTVar (outstandingReqs state) pendingReqs

  shouldIDie conn

updateConnState :: (MonadIO m, Universe m) => IMAPConnection -> CommandResult -> m ()
updateConnState conn command = do
  let connState = connectionState conn

  case command of
    Untagged u -> case u of
                    OKResult _ -> liftIO . atomically $ writeTVar connState Connected
                    Bye -> liftIO . atomically $ writeTVar connState Disconnected

                    _ -> return ()
    _ -> return ()

shouldIDie :: (MonadIO m, Universe m) => IMAPConnection -> m ()
shouldIDie conn = liftIO $ do
  threadId <- atomically . readTVar . serverWatcherThread . imapState $ conn
  connState <- atomically . readTVar $ connectionState conn

  if isDisconnected connState && isJust threadId
    then killThread $ fromJust threadId
    else return ()

dispatchTagged :: (MonadIO m, Universe m) => [ResponseRequest] ->
  TaggedResult -> m [ResponseRequest]
dispatchTagged requests response = do
  let reqId = commandId response
  let pendingRequest = L.find (\r -> respRequestId r == reqId) requests

  if isJust pendingRequest
    then liftIO . atomically $ do
      writeTQueue (responseQueue . fromJust $ pendingRequest) $ Tagged response
    else liftIO $ errorM "RequestWatcher" "Received a reply for an unknown request"

  return $ if isJust pendingRequest
            then filter (/= fromJust pendingRequest) requests
            else requests

dispatchUntagged :: (MonadIO m, Universe m) => IMAPConnection ->
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

type ParseResult = Either ErrorMessage CommandResult
parseChunk :: (BSC.ByteString -> Result ParseResult) ->
              BSC.ByteString ->
              ((Maybe ParseResult, Maybe (BSC.ByteString -> Result ParseResult)), BSC.ByteString)
parseChunk parser chunk =
    case parser chunk of
      Fail left _ msg -> ((Just . Left . T.pack $ msg, Nothing), omitOneLine left)
      Partial continuation -> ((Nothing, Just continuation), BS.empty)
      Done left result -> ((Just result, Nothing), left)

getParsedChunk :: (MonadIO m, Universe m) => Connection ->
                  (BSC.ByteString -> Result ParseResult) ->
                  m ParseResult
getParsedChunk conn parser = do
  (parsed, cont) <- connectionGetChunk'' conn $ parseChunk parser

  if isJust cont
    then getParsedChunk conn $ fromJust cont
    else return . fromJust $ parsed

-- |Reject all outstanding requests with the exception handler, close the watcher
handleExceptions :: (MonadIO m, Universe m, MonadCatch m) => IMAPConnection ->
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
    resultRest = BSC.append "Exception caught " (BSC.pack . show $ e)
  }
  liftIO . atomically $ mapM_ (sendResponse reply) requests

  if isJust threadId
   then liftIO . killThread $ fromJust threadId
   else return ()

sendResponse :: TaggedResult -> ResponseRequest -> STM ()
sendResponse response request = writeTQueue (responseQueue request) $ Tagged response
