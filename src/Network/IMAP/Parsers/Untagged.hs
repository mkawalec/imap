module Network.IMAP.Parsers.Untagged where

import Network.IMAP.Types
import Network.IMAP.Parsers.Utils

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text.Encoding (decodeUtf8)
import Data.Either.Combinators (mapBoth, mapRight)

import Control.Applicative
import Control.Monad (liftM, (>=>))

parseConnectionState :: ByteString -> (T.Text -> UntaggedResult) -> Parser UntaggedResult
parseConnectionState stateName constructor = do
  string stateName
  char ' '
  contents <- AP.takeWhile (/= '\r')
  return . constructor . decodeUtf8 $ contents

parseOk :: Parser UntaggedResult
parseOk = parseConnectionState "OK" OKResult

parseNo :: Parser UntaggedResult
parseNo = parseConnectionState "NO" NOResult

parseBad :: Parser UntaggedResult
parseBad = parseConnectionState "BAD" BADResult

parseExtension :: Parser (Either ErrorMessage UntaggedResult)
parseExtension = do
  extensionName <- AP.takeWhile1 isAtomChar
  char ' '

  payload <- ((liftA ExtLabels <$> Right) <$>
              (char '(' *> AP.sepBy parseLabel (char ' ') <* char ')')
             )
                <|>
             (liftA ExtInt <$> parseNumber id "" "")
  return $! (Extension extensionName) <$> payload

parseFlag :: Parser Flag
parseFlag = do
  char '\\'
  flagName <- takeWhile1 (\c -> isAtomChar c || c == '*')
  case flagName of
            "Seen" -> return FSeen
            "Answered" -> return FAnswered
            "Flagged" -> return FFlagged
            "Deleted" -> return FDeleted
            "Draft" -> return FDraft
            "Recent" -> return FRecent
            "*" -> return FAny
            _ -> return . FOther . decodeUtf8 $ flagName

parseFlagKeyword :: Parser Flag
parseFlagKeyword = do
  flagText <- AP.takeWhile1 isAtomChar
  return . FOther . decodeUtf8 $ flagText

parseFlagList :: Parser [Flag]
parseFlagList = char '(' *>
                (parseFlag <|> parseFlagKeyword) `sepBy` char ' '
                <* char ')'

parseFlags :: Parser (Either ErrorMessage UntaggedResult)
parseFlags = Right . Flags <$> (string "FLAGS " *> parseFlagList)

parseExists :: Parser (Either ErrorMessage UntaggedResult)
parseExists = parseNumber Exists "" "EXISTS"

parseBye :: Parser UntaggedResult
parseBye = string "BYE" *> AP.takeWhile (/= '\r') *> return Bye

parseRecent :: Parser (Either ErrorMessage UntaggedResult)
parseRecent = parseNumber Recent "" "RECENT"

parseOkResp :: Parser a -> Parser a
parseOkResp innerParser = string "OK [" *> innerParser <* string "]"

parseUnseen :: Parser (Either ErrorMessage UntaggedResult)
parseUnseen = parseOkResp $
  (toInt >=> (Right . Unseen)) <$>
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

parseStatusItem :: Parser (Either ErrorMessage UntaggedResult)
parseStatusItem = do
  anyChar
  itemName <- liftM decodeUtf8 $ AP.takeWhile1 isAtomChar
  char ' '
  value <- liftM decodeUtf8 $ AP.takeWhile1 isDigit

  let decodingError = T.concat ["Error decoding '", value, "' as integer"]
  let valAsNumber = mapBoth (const decodingError) fst $ TR.decimal value

  return $ valAsNumber >>= \n -> case itemName of
    "MESSAGES" -> Right $ Messages n
    "RECENT" -> Right $ Recent n
    "UIDNEXT" -> Right $ UIDNext n
    "UIDVALIDITY" -> Right $ UIDValidity n
    "UNSEEN" -> Right $ Unseen n
    _ -> Left $ T.concat ["Unknown status item '", itemName, "'"]

parseStatus :: Parser (Either ErrorMessage UntaggedResult)
parseStatus = do
  string "STATUS "
  mailboxName <- liftM decodeUtf8 $ AP.takeWhile1 isAtomChar

  char ' '
  statuses <- parseStatusItem `manyTill` char ')'
  let formattedStatuses = sequence statuses

  return $ liftM (StatusR mailboxName) formattedStatuses

parseCapabilityList :: Parser (Either ErrorMessage UntaggedResult)
parseCapabilityList = do
  string "CAPABILITY "
  caps <- (parseCapabilityWithValue <|> parseNamedCapability) `sepBy` char ' '
  return . mapRight Capabilities $ sequence caps

parseCapabilityWithValue :: Parser (Either ErrorMessage Capability)
parseCapabilityWithValue = do
  name <- liftM decodeUtf8 (AP.takeWhile1 isAtomChar)
  char '='
  value <- AP.takeWhile1 isAtomChar

  let decodedValue = decodeUtf8 value
  let decodingError = T.concat ["Error decoding '", decodedValue, "' as integer"]
  let valAsNumber = mapBoth (const decodingError) fst $ TR.decimal decodedValue
  case T.toLower name of
    "compress" -> return . Right . CCompress $ decodedValue
    "utf8" -> return . Right . CUtf8 $ decodedValue
    "auth" -> return . Right . CAuth $ decodedValue
    "appendlimit" -> return $ liftM CAppendLimit valAsNumber
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

parseExpunge :: Parser (Either ErrorMessage UntaggedResult)
parseExpunge = do
  msgId <- AP.takeWhile1 isDigit
  string " EXPUNGE"

  return $ liftM Expunge (toInt msgId)

parseSearchResult :: Parser (Either ErrorMessage UntaggedResult)
parseSearchResult = do
  string "SEARCH"
  msgIds <- AP.takeWhile isDigit `sepBy` char ' '
  let parsedIds = mapM toInt (filter (/= "") msgIds)
  return $ liftM Search parsedIds
