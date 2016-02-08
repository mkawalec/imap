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
import Data.Maybe (fromJust, isJust)
import Data.Either.Combinators (mapBoth, mapRight, isRight,
  fromRight', fromLeft', rightToMaybe)
import qualified Debug.Trace as DT

import Control.Applicative
import Control.Monad (mzero, liftM)

parseReply :: Parser (Either ErrorMessage [CommandResult])
parseReply = parseFetch <|> parseLine

parseLine :: Parser (Either ErrorMessage [CommandResult])
parseLine = do
  parsed <- parseUntagged <|> parseTagged
  string "\r\n"
  return $ parsed >>= \p -> return [p]

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
            (Right <$> parseListLikeResp "LIST") <|>
            (Right <$> parseListLikeResp "LSUB") <|>
            parseStatus <|>
            parseExpunge <|>
            parseSearchResult

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

parseNumber :: (Int -> a) -> BSC.ByteString ->
  BSC.ByteString -> Parser (Either ErrorMessage a)
parseNumber constructor prefix postfix = do
  if not . BSC.null $ prefix
    then string prefix <* word8 _space
    else return BSC.empty
  count <- takeWhile1 isDigit
  if not . BSC.null $ postfix
    then word8 _space *> string postfix
    else return BSC.empty

  return $ toInt count >>= return . constructor

parseFetch :: Parser (Either ErrorMessage [CommandResult])
parseFetch = do
  string "* "
  msgId <- (AP.takeWhile1 isDigit >>= return . toInt)
  let unpackedId = rightToMaybe msgId
  string " FETCH ("
  parsedFetch <- parseSpecifier
  DT.traceShow parsedFetch $ return ()
  return $ (mapM id parsedFetch) >>= return . map Untagged

parseSpecifier :: Parser [Either ErrorMessage UntaggedResult]
parseSpecifier = do
  nextChar <- AP.peekWord8
  if (not . isJust $ nextChar) || (fromJust nextChar == _cr)
    then return []
    else do
      nextRes <- ((Right <$> parseEnvelope) <|>
                  parseFlags <|>
                  (Right <$> parseInternalDate) <|>
                  parseNumber Size "RFC822.SIZE" "" <|>
                  ((string "BODY[" <|> string "RFC822.HEADER"
                   <|> string "RFC822.TEXT" <|> string "RFC822") *> parseBody) <|>
                  parseNumber UID "UID" "" <|>
                  parseBodyStructures)

      DT.traceShow nextRes $ return ()
      (nextRes:) <$> (AP.anyWord8 *> parseSpecifier)

parseInternalDate :: Parser UntaggedResult
parseInternalDate = liftM InternalDate $ string "INTERNALDATE " *> parseQuotedText

parseBody :: Parser (Either ErrorMessage UntaggedResult)
parseBody = do
  AP.takeWhile (/= _braceleft)
  word8 _braceleft
  size <- AP.takeWhile1 isDigit
  string "}\r\n"

  let parsedSize = toInt size
  if isRight parsedSize
    then do
      msg <- AP.take (fromRight' parsedSize)
      return . Right . Body $ msg
    else return . Left $ fromLeft' parsedSize

parseBodyStructures :: Parser (Either ErrorMessage UntaggedResult)
parseBodyStructures = do
  string "BODYSTRUCTURE "
  AP.takeWhile (== _parenleft)
  structures <- parseBodyStructure `sepBy` word8 _parenleft
  AP.takeWhile (== _parenright)

  return $ mapM id structures >>= return . BodyStructures

parseBodyStructure :: Parser (Either ErrorMessage BodyStructure)
parseBodyStructure = do
  bodyType' <- nilOrValue parseQuotedText
  word8 _space

  bodySubtype' <- nilOrValue parseQuotedText
  word8 _space

  bodyParams' <- nilOrValue parseBodyParams
  word8 _space

  bodyId' <- nilOrValue parseQuotedText
  word8 _space

  bodyDescription' <- nilOrValue parseQuotedText
  word8 _space

  bodyEncoding' <- nilOrValue parseQuotedText
  word8 _space

  bodySize' <- parseNumber id "" ""
  eatUntilClosingParen

  return $ bodySize' >>= \size -> return BodyStructure {
    bodyType = bodyType',
    bodySubtype = bodySubtype',
    bodyParams = bodyParams',
    bodyId = bodyId',
    bodyDescription = bodyDescription',
    bodyEncoding = bodyEncoding',
    bodySize = size
  }

eatUntilClosingParen :: Parser BSC.ByteString
eatUntilClosingParen = scan 0 hadClosedAllParens <* word8 _parenright

hadClosedAllParens :: Int -> Word8 -> Maybe Int
hadClosedAllParens openingParenCount char =
  if char == _parenright
    then if openingParenCount == 0
      then Nothing
      else Just $ openingParenCount - 1
  else if char == _parenleft
    then Just $ openingParenCount + 1
    else Just openingParenCount


parseBodyParams :: Parser [BodyParam]
parseBodyParams = string "(" *> parseBodyParam `sepBy` word8 _space <* string ")"

parseBodyParam :: Parser BodyParam
parseBodyParam = do
  paramName' <- parseQuotedText
  word8 _space
  paramValue' <- parseQuotedText

  return $ BodyParam paramName' paramValue'


parseEnvelope :: Parser UntaggedResult
parseEnvelope = do
  string "ENVELOPE ("
  date <- nilOrValue parseQuotedText
  word8 _space

  subject <- nilOrValue parseQuotedText
  word8 _space

  from <- nilOrValue parseEmailList
  word8 _space

  sender <- nilOrValue parseEmailList
  word8 _space

  replyTo <- nilOrValue parseEmailList
  word8 _space

  to <- nilOrValue parseEmailList
  word8 _space

  cc <- nilOrValue parseEmailList
  word8 _space

  bcc <- nilOrValue parseEmailList
  word8 _space

  inReplyTo <- nilOrValue parseQuotedText
  word8 _space

  messageId <- nilOrValue parseQuotedText
  string ")"

  return Envelope {
    eDate = date,
    eSubject = subject,
    eFrom = from,
    eSender = sender,
    eReplyTo = replyTo,
    eTo = to,
    eCC = cc,
    eBCC = bcc,
    eInReplyTo = inReplyTo,
    eMessageId = messageId
  }


parseEmailList :: Parser [EmailAddress]
parseEmailList = string "(" *> parseEmail `sepBy` word8 _space <* string ")"

parseEmail :: Parser EmailAddress
parseEmail = do
  string "(\""
  label <- AP.takeWhile1 (/= _quotedbl)
  string "\" NIL \""

  emailUsername <- AP.takeWhile1 (/= _quotedbl)
  string "\" \""
  emailDomain <- AP.takeWhile1 (/= _quotedbl)
  string "\")"
  let fullAddr = decodeUtf8 $ BSC.concat [emailUsername, "@", emailDomain]

  return $ EmailAddress (decodeUtf8 label) fullAddr


nilOrValue :: Parser a -> Parser (Maybe a)
nilOrValue parser = rightToMaybe <$> AP.eitherP (string "NIL") parser

parseQuotedText :: Parser T.Text
parseQuotedText = do
  word8 _quotedbl
  date <- AP.takeWhile1 (/= _quotedbl)
  word8 _quotedbl

  return . decodeUtf8 $ date

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

parseStatusItem :: Parser (Either ErrorMessage UntaggedResult)
parseStatusItem = do
  anyWord8
  itemName <- (AP.takeWhile1 isAtomChar >>= return . decodeUtf8)
  string " "
  value <- (AP.takeWhile1 isDigit >>= return . decodeUtf8)

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
  inboxName <- (AP.takeWhile1 isAtomChar >>= return . decodeUtf8)

  string " "
  statuses <- parseStatusItem `manyTill` string ")"
  let formattedStatuses = mapM id statuses

  return $ formattedStatuses >>= return . StatusR inboxName

parseListLikeResp :: BSC.ByteString -> Parser UntaggedResult
parseListLikeResp prefix = do
  string prefix
  string " ("
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

parseCapabilityWithValue :: Parser (Either ErrorMessage Capability)
parseCapabilityWithValue = do
  name <- (AP.takeWhile1 isAtomChar >>= return . decodeUtf8)
  word8 _equal
  value <- AP.takeWhile1 isAtomChar

  let decodedValue = decodeUtf8 value
  let decodingError = T.concat ["Error decoding '", decodedValue, "' as integer"]
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

parseExpunge :: Parser (Either ErrorMessage UntaggedResult)
parseExpunge = do
  msgId <- AP.takeWhile1 isDigit
  string " EXPUNGE"

  return $ toInt msgId >>= return . Expunge

parseSearchResult :: Parser (Either ErrorMessage UntaggedResult)
parseSearchResult = do
  string "SEARCH "
  msgIds <- (AP.takeWhile1 isDigit) `sepBy` word8 _space
  let parsedIds = mapM id $ map toInt msgIds
  return $ parsedIds >>= return . Search

isAtomChar :: Word8 -> Bool
isAtomChar c = isLetter c || isNumber c || c == _hyphen || c == _quotedbl || c == _period

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]
