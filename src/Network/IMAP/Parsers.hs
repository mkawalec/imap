module Network.IMAP.Parsers where

import Network.IMAP.Types
import Network.IMAP.Parsers.Fetch
import Network.IMAP.Parsers.Utils
import Network.IMAP.Parsers.Untagged


import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8
import qualified Debug.Trace as DT

import Control.Applicative

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
