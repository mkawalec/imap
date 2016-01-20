module Network.IMAP where

import Network.Connection
import System.Random
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT
import Data.Maybe (isJust, fromJust)

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8
import qualified Data.List as L

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM

import Data.Either (isRight)
import Data.Either.Combinators (fromRight', mapLeft)
import Control.Concurrent (forkIO, ThreadId, myThreadId)
import Control.Monad (join)

type ErrorMessage = T.Text
type RequestId = BSC.ByteString

data ConnectionState = Connected | Authenticated | Selected T.Text
data IMAPConnection = IMAPConnection {
  rawConnection :: !Connection,
  connectionState :: !ConnectionState,
  commandReplies :: TVar (M.Map RequestId RequestResponse),
  responseRequests :: TQueue ResponseRequest,
  serverWatcherThread :: ThreadId,
  untaggedQueue :: RQ.RollingQueue UntaggedResult
}

data ResponseRequest = ResponseRequest {
  requestResponse :: TMVar RequestResponse,
  respRequestId :: RequestId
} deriving (Eq)

data CommandState = OK | NO | BAD deriving (Show)

data Flag = FSeen | FAnswered | FFlagged | FDeleted | FDraft | FRecent
  deriving (Show)

data TaggedResult = TaggedResult {
                      requestId :: RequestId,
                      commandState :: !CommandState,
                      commandRest :: BSC.ByteString
                    } deriving (Show)

data UntaggedResult = Flags [Flag]
                    | Exists Int
                    | Recent Int
                    | Unseen Int
                    | PermanentFlags [Flag]
                    | UIDNext Int
                    deriving (Show)

data CommandResult = Tagged TaggedResult | Untagged UntaggedResult
  deriving (Show)

data RequestResponse = RequestResponse {
  untaggedResults :: [UntaggedResult],
  taggedResult :: Maybe TaggedResult
} deriving (Show)

dispatchTagged :: IMAPConnection -> [ResponseRequest] -> TaggedResult -> IO [ResponseRequest]
dispatchTagged conn outstandingReqs response = do
  let reqId = requestId response
  let pendingRequest = L.find (\r -> respRequestId r == reqId) outstandingReqs
  let replies = commandReplies conn

  if isJust pendingRequest
    then atomically $ do
      repliesMap <- readTVar replies
      let reply = if M.member reqId repliesMap
                    then (repliesMap M.! reqId) {taggedResult = Just response}
                    else RequestResponse {untaggedResults = [], taggedResult = Just response}
      putTMVar (requestResponse . fromJust $ pendingRequest) reply
      writeTVar (commandReplies conn) $ M.delete reqId repliesMap
    else atomically $ do
      repliesMap <- readTVar replies
      let wrappedResponse = RequestResponse [] $ Just response
      writeTVar replies $ M.insert reqId wrappedResponse repliesMap

  return $ if isJust pendingRequest
            then filter (== fromJust pendingRequest) outstandingReqs
            else outstandingReqs

dispatchUntagged :: IMAPConnection ->
                    [ResponseRequest] ->
                    UntaggedResult ->
                    IO [ResponseRequest]
dispatchUntagged conn outstandingReqs response = do
  if null outstandingReqs
    then atomically $ RQ.write (untaggedQueue conn) response
    else atomically $ do
      let reqId = respRequestId . head $ outstandingReqs
      repliesMap <- readTVar $ commandReplies conn
      let reply = if M.member reqId repliesMap
                    then repliesMap M.! reqId
                    else RequestResponse [] Nothing
      let updatedReply = reply {untaggedResults = response:(untaggedResults reply)}
      writeTVar (commandReplies conn) $ M.insert reqId updatedReply repliesMap
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


requestWatcher :: IMAPConnection -> [ResponseRequest] -> IO ()
requestWatcher conn knownReqs = do
  line <- connectionGetLine 100000 (rawConnection conn)
  let parsedLine = join $ mapLeft T.pack (AP.parseOnly parseLine line)

  newReqs <- atomically $ getOutstandingReqs (responseRequests conn)
  let outstandingReqs = knownReqs ++ newReqs

  DT.traceShow parsedLine $ DT.traceShow line $ DT.traceShow (length outstandingReqs) $ return ()
  nOutReqs <- if isRight parsedLine
                then do
                  let parsed = fromRight' parsedLine

                  case parsed of
                    Tagged t -> dispatchTagged conn outstandingReqs t
                    Untagged u -> dispatchUntagged conn outstandingReqs u
                else return outstandingReqs
  DT.traceShow (length nOutReqs) $ return ()
  requestWatcher conn nOutReqs

connectServer :: IO IMAPConnection
connectServer = do
  context <- initConnectionContext
  let params = ConnectionParams "imap.gmail.com" 993 Nothing Nothing
  let tlsSettings = TLSSettingsSimple False False False

  connection <- connectTo context params
  connectionSetSecure context connection tlsSettings

  untaggedRespsQueue <- RQ.newIO 20
  repliesMap <- newTVarIO M.empty
  responseRequestsQueue <- newTQueueIO
  currentThreadId <- myThreadId

  let conn = IMAPConnection {
    rawConnection = connection,
    connectionState = Connected,
    commandReplies =  repliesMap,
    responseRequests = responseRequestsQueue,
    serverWatcherThread = currentThreadId,
    untaggedQueue = untaggedRespsQueue
  }
  watcherThreadId <- forkIO $ requestWatcher conn []

  return conn {
    serverWatcherThread = watcherThreadId
  }

genRequestId :: IO BSC.ByteString
genRequestId = do
  randomGen <- newStdGen
  return $ BSC.pack . Prelude.take 9 $ randomRs ('a', 'z') randomGen

sendCommand :: IMAPConnection -> BSC.ByteString -> IO RequestResponse
sendCommand conn command = do
  requestId <- genRequestId
  let commandLine = BSC.concat [requestId, " ", command, "\r\n"]

  connectionPut (rawConnection conn) commandLine
  responseWrapper <- atomically $ newEmptyTMVar

  let responseRequest = ResponseRequest responseWrapper requestId
  atomically $ writeTQueue (responseRequests conn) responseRequest
  atomically $ takeTMVar responseWrapper

login :: IMAPConnection -> T.Text -> T.Text -> IO RequestResponse
login conn username password = sendCommand conn . encodeUtf8 $
  T.intercalate " " ["LOGIN", escapeText username, escapeText password]

escapeText :: T.Text -> T.Text
escapeText t = T.replace "{" "\\{" $
             T.replace "}" "\\}" $
             T.replace "\"" "\\\"" $
             T.replace "\\" "\\\\" t

parseLine :: Parser (Either ErrorMessage CommandResult)
parseLine = do
  parsed <- parseUntagged <|> parseTagged
  string "\r"
  return parsed

parseTagged :: Parser (Either ErrorMessage CommandResult)
parseTagged = do
  requestId <- takeWhile1 isLetter
  word8 _space

  commandState <- takeWhile1 isLetter
  word8 _space

  rest <- takeWhile1 (/= _cr)
  let state = case commandState of
                "OK" -> OK
                "NO" -> NO
                "BAD" -> BAD
                _ -> BAD

  return . Right . Tagged $ TaggedResult requestId state rest

parseUntagged :: Parser (Either ErrorMessage CommandResult)
parseUntagged = do
  string "* "
  result <- parseFlags <|>
            parseExists <|>
            parseRecent <|>
            parseUnseen <|>
            (Right <$> parsePermanentFlags) <|>
            parseUidNext <|>
            parseUidValidity

  -- Take the rest
  _ <- AP.takeWhile (/= _cr)
  return $ result >>= Right . Untagged

parseFlag :: Parser Flag
parseFlag = do
  word8 _backslash
  flagName <- takeWhile1 isLetter
  return $ case flagName of
            "Seen" -> FSeen
            "Answered" -> FAnswered
            "Flagged" -> FFlagged
            "Deleted" -> FDeleted
            "Draft" -> FDraft
            "Recent" -> FRecent

parseFlagList :: Parser [Flag]
parseFlagList = word8 _parenleft *>
                parseFlag `sepBy` word8 _space
                <* word8 _parenright

parseFlags :: Parser (Either ErrorMessage UntaggedResult)
parseFlags = Right . Flags <$> (string "FLAGS " *> parseFlagList)

parseNumber :: (Int -> UntaggedResult) -> BSC.ByteString -> BSC.ByteString -> Parser (Either ErrorMessage UntaggedResult)
parseNumber constructor prefix postfix = do
  if not . BSC.null $ prefix
    then string prefix <* word8 _space
    else return BSC.empty
  count <- takeWhile1 isDigit
  DT.traceShow count $ if not . BSC.null $ postfix
    then word8 _space *> string postfix
    else return BSC.empty

  return $ toInt count >>= return . constructor

parseExists :: Parser (Either ErrorMessage UntaggedResult)
parseExists = parseNumber Exists "" "EXISTS"

parseRecent :: Parser (Either ErrorMessage UntaggedResult)
parseRecent = parseNumber Recent "" "RECENT"

parseOkResp :: Parser a -> Parser a
parseOkResp innerParser = string "OK [" *> innerParser <* string "]"

parseUnseen :: Parser (Either ErrorMessage UntaggedResult)
parseUnseen = parseOkResp $
  (\x -> toInt x >>= Right . Unseen) <$>
  (string "UNSEEN " *> takeWhile1 isDigit)

parsePermanentFlags :: Parser UntaggedResult
parsePermanentFlags = parseOkResp $
  PermanentFlags <$> (string "PERMANENTFLAGS " *> parseFlagList)

parseUidNext :: Parser (Either ErrorMessage UntaggedResult)
parseUidNext = parseOkResp $ parseNumber UIDNext "UIDNEXT" ""

parseUidValidity :: Parser (Either ErrorMessage UntaggedResult)
parseUidValidity = parseOkResp $ parseNumber UIDNext "UIDVALIDITY" ""

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]
