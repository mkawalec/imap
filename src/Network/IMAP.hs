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

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Data.Either (isRight)
import Data.Either.Combinators (fromRight', mapLeft)
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (join)

type ErrorMessage = T.Text
data ConnectionState = Connected | Authenticated | Selected T.Text
data IMAPConnection = IMAPConnection {
  rawConnection :: !Connection,
  connectionState :: !ConnectionState,
  commandReplies :: M.Map BSC.ByteString CommandResult,
  serverWatcherThread :: ThreadId,
  untaggedQueue :: RQ.RollingQueue CommandResult
}

data CommandState = OK | NO | BAD deriving (Show)

data Flag = FSeen | FAnswered | FFlagged | FDeleted | FDraft | FRecent
  deriving (Show)

data CommandResult = TaggedResult {
                      commandId :: BSC.ByteString,
                      commandState :: !CommandState,
                      commandRest :: BSC.ByteString
                    }
                   | Flags [Flag]
                   | Exists Int
                   | Recent Int
                   | Unseen Int
                   | PermanentFlags [Flag]
                   | UIDNext Int


requestWatcher :: RQ.RollingQueue CommandResult ->
                  TQueue CommandResult ->
                  Connection ->
                  IO ()
requestWatcher untaggedQueue taggedQueue imapConnection = do
  line <- connectionGetLine 100000 imapConnection
  let parsedLine = join $ mapLeft T.pack (AP.parseOnly parseLine line)

  if isRight parsedLine
    then do
      let parsed = fromRight' parsedLine
      atomically $ case parsed of
                    TaggedResult _ _ _ -> writeTQueue taggedQueue parsed
                    _ -> RQ.write untaggedQueue parsed
    else return ()
  requestWatcher untaggedQueue taggedQueue imapConnection

connectServer :: IO IMAPConnection
connectServer = do
  context <- initConnectionContext
  let params = ConnectionParams "imap.gmail.com" 993 Nothing Nothing
  let tlsSettings = TLSSettingsSimple False False False

  connection <- connectTo context params
  connectionSetSecure context connection tlsSettings

  untaggedQueue <- RQ.newIO 20
  taggedQueue <- newTQueueIO
  watcher <- forkIO $ requestWatcher untaggedQueue taggedQueue connection

  return $ IMAPConnection connection Connected M.empty watcher untaggedQueue

genCommandId :: IO BSC.ByteString
genCommandId = do
  randomGen <- newStdGen
  return $ BSC.pack . Prelude.take 9 $ randomRs ('a', 'z') randomGen

sendCommand :: IMAPConnection -> BSC.ByteString -> IO BSC.ByteString
sendCommand conn command = do
  commandId <- genCommandId
  let commandLine = BSC.concat [commandId, " ", command, "\r\n"]

  connectionPut (rawConnection conn) commandLine
  return commandId

login :: IMAPConnection -> T.Text -> T.Text -> IO BSC.ByteString
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
  string "\r\n"
  return parsed

parseTagged :: Parser (Either ErrorMessage CommandResult)
parseTagged = do
  commandId <- takeWhile1 isLetter
  word8 _space

  commandState <- takeWhile1 isLetter
  word8 _space

  rest <- takeWhile1 (/= _cr)
  let state = case commandState of
                "OK" -> OK
                "NO" -> NO
                "BAD" -> BAD
                _ -> BAD

  return . Right $ TaggedResult commandId state rest

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
  return result

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

parseFlags :: Parser (Either ErrorMessage CommandResult)
parseFlags = Right . Flags <$> (string "FLAGS " *> parseFlagList)

parseNumber :: (Int -> CommandResult) -> BSC.ByteString -> BSC.ByteString -> Parser (Either ErrorMessage CommandResult)
parseNumber constructor prefix postfix = do
  if not . BSC.null $ prefix
    then string prefix <* word8 _space
    else return BSC.empty
  count <- takeWhile1 isDigit
  DT.traceShow count $ if not . BSC.null $ postfix
    then word8 _space *> string postfix
    else return BSC.empty

  return $ toInt count >>= return . constructor

parseExists :: Parser (Either ErrorMessage CommandResult)
parseExists = parseNumber Exists "" "EXISTS"

parseRecent :: Parser (Either ErrorMessage CommandResult)
parseRecent = parseNumber Recent "" "RECENT"

parseOkResp :: Parser a -> Parser a
parseOkResp innerParser = string "OK [" *> innerParser <* string "]"

parseUnseen :: Parser (Either ErrorMessage CommandResult)
parseUnseen = parseOkResp $
  (\x -> toInt x >>= Right . Unseen) <$>
  (string "UNSEEN " *> takeWhile1 isDigit)

parsePermanentFlags :: Parser CommandResult
parsePermanentFlags = parseOkResp $
  PermanentFlags <$> (string "PERMANENTFLAGS " *> parseFlagList)

parseUidNext :: Parser (Either ErrorMessage CommandResult)
parseUidNext = parseOkResp $ parseNumber UIDNext "UIDNEXT" ""

parseUidValidity :: Parser (Either ErrorMessage CommandResult)
parseUidValidity = parseOkResp $ parseNumber UIDNext "UIDVALIDITY" ""

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]
