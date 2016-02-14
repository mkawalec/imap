module Network.IMAP.Parsers.Fetch where

import Network.IMAP.Types
import Network.IMAP.Parsers.Utils
import Network.IMAP.Parsers.Untagged (parseFlags)

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromJust, isJust)
import Data.Either.Combinators (isRight, fromRight', fromLeft')
import qualified Debug.Trace as DT

import Control.Applicative
import Control.Monad (liftM)


parseFetch :: Parser (Either ErrorMessage [CommandResult])
parseFetch = do
  string "* "
  msgId <- (AP.takeWhile1 isDigit >>= return . toInt)
  let msgId' = msgId >>= Right . MessageId
  string " FETCH ("

  parsedFetch <- parseSpecifier

  DT.traceShow parsedFetch $ return ()
  let allInOneEither = mapM id $ msgId':parsedFetch
  return $ allInOneEither >>= return . map Untagged

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
                  Right <$> parseBodyStructure)

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

parseBodyStructure :: Parser UntaggedResult
parseBodyStructure = do
  string "BODYSTRUCTURE "
  structure <- eatUntilClosingParen
  word8 _parenright
  return . BodyStructure $ BSC.snoc structure ')'


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
