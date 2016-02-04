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
import qualified Debug.Trace as DT

import System.Log.Logger (errorM)


requestWatcher :: (MonadIO m, Universe m) => IMAPConnection -> [ResponseRequest] -> m ()
requestWatcher conn knownReqs = do
  let state = imapState conn

  parsedLine <- getParsedChunk (rawConnection state) (AP.parse parseLine)
  newReqs <- liftIO . atomically $ getOutstandingReqs (responseRequests state)
  let outstandingReqs = knownReqs ++ newReqs

  nOutReqs <- if isRight parsedLine
                then do
                  let parsed = fromRight' parsedLine

                  updateConnState conn parsed
                  case parsed of
                    Tagged t -> dispatchTagged conn outstandingReqs t
                    Untagged u -> dispatchUntagged conn outstandingReqs u
                else return outstandingReqs
  requestWatcher conn nOutReqs

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
  threadId <- atomically . readTVar $ serverWatcherThread conn
  connState <- atomically . readTVar $ connectionState conn
  atomically $ writeTVar (serverWatcherThread conn) Nothing

  if isDisconnected connState && isJust threadId
    then liftIO . killThread $ fromJust threadId
    else return ()

dispatchTagged :: (MonadIO m, Universe m) => IMAPConnection ->
  [ResponseRequest] -> TaggedResult -> m [ResponseRequest]
dispatchTagged conn outstandingReqs response = do
  let reqId = commandId response
  let pendingRequest = L.find (\r -> respRequestId r == reqId) outstandingReqs

  if isJust pendingRequest
    then liftIO . atomically $ do
      writeTQueue (responseQueue . fromJust $ pendingRequest) $ Tagged response
    else liftIO $ errorM "RequestWatcher" "Received a reply for an unknown request"

  shouldIDie conn
  return $ if isJust pendingRequest
            then filter (/= fromJust pendingRequest) outstandingReqs
            else outstandingReqs

dispatchUntagged :: (MonadIO m, Universe m) => IMAPConnection ->
                    [ResponseRequest] ->
                    UntaggedResult ->
                    m [ResponseRequest]
dispatchUntagged conn outstandingReqs response = do
  if null outstandingReqs
    then liftIO . atomically $ RQ.write (untaggedQueue conn) response
    else liftIO . atomically $ do
      let responseQ = responseQueue . head $ outstandingReqs
      writeTQueue responseQ $ Untagged response
  return outstandingReqs

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
parseChunk parser chunk = DT.traceShow chunk $
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
