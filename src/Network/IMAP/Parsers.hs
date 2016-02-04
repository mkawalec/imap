module Network.IMAP.Parsers where

import Network.IMAP.Types

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Either.Combinators (mapBoth, mapRight)
import qualified Debug.Trace as DT

import Control.Applicative
import Control.Monad (mzero)

parseLine :: Parser (Either ErrorMessage CommandResult)
parseLine = do
  parsed <- parseUntagged <|> parseTagged
  string "\r\n"
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
            parseHighestModSeq <|>
            parseRecent <|>
            parseUnseen <|>
            (Right <$> parsePermanentFlags) <|>
            parseUidNext <|>
            parseUidValidity <|>
            parseCapabilityList <|>
            (Right <$> parseOk) <|>
            (Right <$> parseBye) <|>
            (Right <$> parseListResp)

  -- Take the rest
  _ <- AP.takeWhile (/= _cr)
  return $ result >>= Right . Untagged

parseOk :: Parser UntaggedResult
parseOk = do
  string "OK "
  contents <- AP.takeWhile (/= _cr)
  return . OKResult . decodeUtf8 $ contents

parseFlag :: Parser Flag
parseFlag = do
  word8 _backslash
  flagName <- takeWhile1 (\c -> isLetter c || c == _asterisk)
  case flagName of
            "Seen" -> return FSeen
            "Answered" -> return FAnswered
            "Flagged" -> return FFlagged
            "Deleted" -> return FDeleted
            "Draft" -> return FDraft
            "Recent" -> return FRecent
            "*" -> return FAny
            _ -> mzero

parseWeirdFlag :: Parser Flag
parseWeirdFlag = do
  flagText <- AP.takeWhile1 (\c -> isLetter c || c == _dollar)
  return . FOther . decodeUtf8 $ flagText

parseFlagList :: Parser [Flag]
parseFlagList = word8 _parenleft *>
                (parseFlag <|> parseWeirdFlag) `sepBy` word8 _space
                <* word8 _parenright

parseFlags :: Parser (Either ErrorMessage UntaggedResult)
parseFlags = Right . Flags <$> (string "FLAGS " *> parseFlagList)

parseNumber :: (Int -> UntaggedResult) -> BSC.ByteString ->
  BSC.ByteString -> Parser (Either ErrorMessage UntaggedResult)
parseNumber constructor prefix postfix = do
  if not . BSC.null $ prefix
    then string prefix <* word8 _space
    else return BSC.empty
  count <- takeWhile1 isDigit
  if not . BSC.null $ postfix
    then word8 _space *> string postfix
    else return BSC.empty

  return $ toInt count >>= return . constructor

parseExists :: Parser (Either ErrorMessage UntaggedResult)
parseExists = parseNumber Exists "" "EXISTS"

parseBye :: Parser UntaggedResult
parseBye = string "BYE" *> AP.takeWhile (/= _cr) *> return Bye

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
parseUidValidity = parseOkResp $ parseNumber UIDValidity "UIDVALIDITY" ""

parseHighestModSeq :: Parser (Either ErrorMessage UntaggedResult)
parseHighestModSeq = parseOkResp $ parseNumber HighestModSeq "HIGHESTMODSEQ" ""

parseNameAttribute :: Parser NameAttribute
parseNameAttribute = do
  string "\\"
  name <- AP.takeWhile1 isAtomChar
  return $ case name of
          "Noinferiors" -> Noinferiors
          "Noselect" -> Noselect
          "Marked" -> Marked
          "Unmarked" -> Unmarked
          "HasNoChildren" -> HasNoChildren
          _ -> OtherNameAttr $ decodeUtf8 name

parseListResp :: Parser UntaggedResult
parseListResp = do
  string "LIST ("
  nameAttributes <- parseNameAttribute `sepBy` word8 _space

  string ") \""
  delimiter <- (AP.anyWord8 >>= return . decodeUtf8 . BS.singleton)
  string "\" "
  name <- (AP.takeWhile1 (/= _cr) >>= return . decodeUtf8)

  let actualName = T.dropAround (== '"') name
  return $ ListR nameAttributes delimiter actualName

parseCapabilityList :: Parser (Either ErrorMessage UntaggedResult)
parseCapabilityList = do
  string "CAPABILITY "
  caps <- (parseCapabilityWithValue <|> parseNamedCapability) `sepBy` word8 _space
  return . (mapRight Capabilities) $ (mapM id) caps

isAtomChar :: Word8 -> Bool
isAtomChar c = isLetter c || isNumber c || c == _hyphen

parseCapabilityWithValue :: Parser (Either ErrorMessage Capability)
parseCapabilityWithValue = do
  name <- (AP.takeWhile1 isAtomChar >>= return . decodeUtf8)
  word8 _equal
  value <- AP.takeWhile1 isAtomChar

  let decodedValue = decodeUtf8 value
  let decodingError = T.concat ["Error deconding '", decodedValue, "' as integer"]
  let valAsNumber = mapBoth (const decodingError) fst $ TR.decimal decodedValue
  case T.toLower name of
    "compress" -> return . Right . CCompress $ decodedValue
    "utf8" -> return . Right . CUtf8 $ decodedValue
    "auth" -> return . Right . CAuth $ decodedValue
    "appendlimit" -> return (valAsNumber >>= return . CAppendLimit)
    _ -> return . Right $ COther name (Just decodedValue)

parseNamedCapability :: Parser (Either ErrorMessage Capability)
parseNamedCapability = do
  name <- AP.takeWhile isAtomChar
  let decodedName = decodeUtf8 name

  return . Right $ case T.toLower decodedName of
    "imap4" -> CIMAP4
    "imap4rev1" -> CIMAP4
    "unselect" -> CUnselect
    "idle" -> CIdle
    "namespace" -> CNamespace
    "quota" -> CQuota
    "id" -> CId
    "children" -> CChildren
    "uidplus" -> CUIDPlus
    "enable" -> CEnable
    "move" -> CMove
    "condstore" -> CCondstore
    "esearch" -> CEsearch
    "list-extended" -> CListExtended
    "list-status" -> CListStatus
    _ -> if T.head decodedName == 'X'
          then CExperimental decodedName
          else COther decodedName Nothing

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]
