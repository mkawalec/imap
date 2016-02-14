module Network.IMAP.Parsers.Utils where

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

eatUntilClosingParen :: Parser BSC.ByteString
eatUntilClosingParen = scan 0 hadClosedAllParens <* word8 _parenright

hadClosedAllParens :: Int -> Word8 -> Maybe Int
hadClosedAllParens openingParenCount char =
  if char == _parenright
    then if openingParenCount == 1
      then Nothing
      else Just $ openingParenCount - 1
  else if char == _parenleft
    then Just $ openingParenCount + 1
    else Just openingParenCount


parseEmailList :: Parser [EmailAddress]
parseEmailList = string "(" *> parseEmail `sepBy` word8 _space <* string ")"

parseEmail :: Parser EmailAddress
parseEmail = do
  string "(\""
  label <- nilOrValue $ AP.takeWhile1 (/= _quotedbl)
  string "\" NIL \""

  emailUsername <- AP.takeWhile1 (/= _quotedbl)
  string "\" \""
  emailDomain <- AP.takeWhile1 (/= _quotedbl)
  string "\")"
  let fullAddr = decodeUtf8 $ BSC.concat [emailUsername, "@", emailDomain]

  return $ EmailAddress (label >>= return . decodeUtf8) fullAddr

nilOrValue :: Parser a -> Parser (Maybe a)
nilOrValue parser = rightToMaybe <$> AP.eitherP (string "NIL") parser

parseQuotedText :: Parser T.Text
parseQuotedText = do
  word8 _quotedbl
  date <- AP.takeWhile1 (/= _quotedbl)
  word8 _quotedbl

  return . decodeUtf8 $ date

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

isAtomChar :: Word8 -> Bool
isAtomChar c = isLetter c || isNumber c || c == _hyphen || c == _quotedbl || c == _period

toInt :: BSC.ByteString -> Either ErrorMessage Int
toInt bs = if null parsed
    then Left errorMsg
    else Right . fst . head $ parsed
  where parsed = reads $ BSC.unpack bs
        errorMsg = T.concat ["Count not parse '", decodeUtf8 bs, "' as an integer"]

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
