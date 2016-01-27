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
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM

import System.Log.Logger (errorM)


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

getParsedChunk :: Connection ->
                  (BSC.ByteString -> Result ParseResult) ->
                  IO ParseResult
getParsedChunk conn parser = do
  (parsed, cont) <- connectionGetChunk' conn $ parseChunk parser

  if isJust cont
    then getParsedChunk conn $ fromJust cont
    else return . fromJust $ parsed

requestWatcher :: IMAPConnection -> [ResponseRequest] -> IO ()
requestWatcher conn knownReqs = do
  let state = imapState conn

  parsedLine <- getParsedChunk (rawConnection state) (AP.parse parseLine)

  newReqs <- atomically $ getOutstandingReqs (responseRequests state)
  let outstandingReqs = knownReqs ++ newReqs

  nOutReqs <- if isRight parsedLine
                then do
                  let parsed = fromRight' parsedLine

                  case parsed of
                    Tagged t -> dispatchTagged state outstandingReqs t
                    Untagged u -> dispatchUntagged conn state outstandingReqs u
                else return outstandingReqs
  requestWatcher conn nOutReqs

constructResponse :: TaggedResult -> [UntaggedResult] -> RequestResponse
constructResponse tagged msgs =
  case resultState tagged of
    OK -> Right msgs
    _  -> Left tagged

dispatchTagged :: IMAPState -> [ResponseRequest] -> TaggedResult -> IO [ResponseRequest]
dispatchTagged state outstandingReqs response = do
  let reqId = commandId response
  let pendingRequest = L.find (\r -> respRequestId r == reqId) outstandingReqs
  let replies = commandReplies state

  if isJust pendingRequest
    then atomically $ do
      repliesMap <- readTVar replies
      let replies = if M.member reqId repliesMap
                      then repliesMap M.! reqId
                      else []
      let completeResponse = constructResponse response replies

      putTMVar (requestResponse . fromJust $ pendingRequest) completeResponse
      writeTVar (commandReplies state) $ M.delete reqId repliesMap
    else errorM "RequestWatcher" "Received a reply for an unknown request"

  return $ if isJust pendingRequest
            then filter (/= fromJust pendingRequest) outstandingReqs
            else outstandingReqs

dispatchUntagged :: IMAPConnection ->
                    IMAPState ->
                    [ResponseRequest] ->
                    UntaggedResult ->
                    IO [ResponseRequest]
dispatchUntagged conn state outstandingReqs response = do
  if null outstandingReqs
    then atomically $ RQ.write (untaggedQueue conn) response
    else atomically $ do
      let reqId = respRequestId . head $ outstandingReqs
      repliesMap <- readTVar $ commandReplies state

      let currentResponses = if M.member reqId repliesMap
                              then repliesMap M.! reqId
                              else []
      let updatedReply = response:currentResponses
      writeTVar (commandReplies state) $ M.insert reqId updatedReply repliesMap
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
