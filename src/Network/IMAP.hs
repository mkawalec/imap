module Network.IMAP where

import Network.Connection
import System.Random
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Control.Applicative
import qualified Data.Map.Strict as M

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8

type ErrorMessage = T.Text
data ConnectionState = Connected | Authenticated | Selected T.Text
data IMAPConnection = IMAPConnection {
  rawConnection :: !Connection,
  connectionState :: !ConnectionState,
  commandReplies :: M.Map BSC.ByteString CommandResult
}

data CommandState = OK | NO | BAD deriving (Show)
data TaggedResult = TaggedResult {
  commandId :: BSC.ByteString,
  commandState :: !CommandState,
  commandRest :: BSC.ByteString
} deriving (Show)

data Flag = FSeen | FAnswered | FFlagged | FDeleted | FDraft | FRecent
  deriving (Show)

data UntaggedResult = Flags [Flag]
                    | Exists Int
                    | Recent Int
                    | Unseen Int
                    | PermanentFlags [Flag]
                    | UIDNext Int
  deriving (Show)

data CommandResult = CommandResult {
  taggedResult :: TaggedResult,
  untaggedResults :: [UntaggedResult]
} deriving (Show)

parseUntagged :: Parser TaggedResult
parseUntagged = do
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

  return $ TaggedResult commandId state rest

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

parseNumber :: (Int -> UntaggedResult) -> BSC.ByteString -> Parser (Either ErrorMessage UntaggedResult)
parseNumber constructor postfix = do
  count <- takeWhile1 isDigit <* word8 _space <* string postfix
  return $ toInt count >>= return . constructor

parseExists :: Parser (Either ErrorMessage UntaggedResult)
parseExists = parseNumber Exists "EXISTS"

parseRecent :: Parser (Either ErrorMessage UntaggedResult)
parseRecent = parseNumber Recent "RECENT"

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
parseUidNext = parseOkResp $
  (\x -> toInt x >>= Right . UIDNext) <$>
  (string "UIDNEXT " *> takeWhile1 isDigit)

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]


--parseTagged :: Parser UntaggedResult
--parseTagged = do
--  word8 _asterisk


connectServer :: IO IMAPConnection
connectServer = do
  context <- initConnectionContext
  let params = ConnectionParams "imap.gmail.com" 993 Nothing Nothing
  let tlsSettings = TLSSettingsSimple False False False
  connection <- connectTo context params

  connectionSetSecure context connection tlsSettings
  return $ IMAPConnection connection Connected M.empty

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
