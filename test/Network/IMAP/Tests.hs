module Network.IMAP.Tests (tests) where

import Network.IMAP
import Network.IMAP.Types
import Test.Utils

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=), assertFailure)
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)

import qualified Data.ByteString.Char8 as B
import qualified Network.IMAP.Parsers.Untagged as U
import qualified Network.IMAP.Parsers.Utils as Utils
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Either.Combinators (fromRight', isLeft)
import Test.QuickCheck
import qualified Data.List as L
import Control.Monad (liftM)
import Data.Char (ord, chr)

lastIsTagged :: [CommandResult] -> (TaggedResult -> Assertion) -> Assertion
lastIsTagged responses testAction =
  case last responses of
    Untagged _ -> assertFailure "The last item should be tagged"
    Tagged r -> testAction r

forUntagged :: [CommandResult] ->
  (UntaggedResult -> Bool) ->
  (UntaggedResult -> Assertion) ->
  Assertion
forUntagged results resultTypeTest action = do
    if isJust isResult then
      action $ fromJust isResult
      else assertFailure "Specified Untagged reply not found"
  where onlyUntagged = map (\(Untagged u) -> u) $ filter isUntagged results
        isResult = L.find resultTypeTest onlyUntagged

testLoginFailure = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "" "NO [ALERT] Invalid credentials (Failure)" $ do
    login conn "a" "b"

  lastIsTagged res $ \r -> resultState r @?= NO

testLoginSuccess = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn
    "* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN \
    \X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE ENABLE MOVE CONDSTORE ESEARCH \
    \UTF8=ACCEPT LIST-EXTENDED LIST-STATUS" "OK a@b authenticated (Success)" $
      login conn "a" "b"

  lastIsTagged res $ \r -> resultState r @?= OK

testFlags = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* FLAGS (\\Answered \\Flagged \\Draft\
    \ \\Deleted \\Seen $NotPhishing $Phishing NonJunk)" "OK command successfull" $
      sendCommand conn "select inbox"

  lastIsTagged res $ \r -> resultState r @?= OK
  forUntagged res isFlags $ \(Flags f) -> do
    (isJust $ L.find isFAnswered f) @?= True
    (isJust $ L.find isFFlagged f) @?= True
    (isJust $ L.find isFDraft f) @?= True
    (isJust $ L.find isFDeleted f) @?= True

    let otherFlags = map (\(FOther t) -> t) $ filter isFOther f
    (isJust $ L.find (=="NonJunk") otherFlags) @?= True
    (isJust $ L.find (=="$Phishing") otherFlags) @?= True

testExists = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* 67 EXISTS" "OK k" $ sendCommand conn "test"
  forUntagged res isExists $ \(Exists count) -> count @?= 67

testRecent = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* 15 RECENT" "OK k" $ sendCommand conn "test"
  forUntagged res isRecent $ \(Recent count) -> count @?= 15


testSearch1 = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* SEARCH 583551 3" "OK UID SEARCH completed" $ sendCommand conn "test"
  forUntagged res isSearch $ \(Search r) -> (length r) @?= 2

testSearch2 = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* SEARCH" "OK UID SEARCH completed" $ sendCommand conn "test"
  forUntagged res isSearch $ \(Search r) -> (length r) @?= 0

testUnseen = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* OK [UNSEEN 15]" "OK k" $ sendCommand conn "test"
  forUntagged res isUnseen $ \(Unseen count) -> count @?= 15

testUIDNext = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* OK [UIDNEXT 923] Predicted next UID." "OK k" $
    sendCommand conn "test"

  forUntagged res isUIDNext $ \(UIDNext nextUID) -> nextUID @?= 923

testPermFlags = do
  conn <- getConn
  (res, _) <- runFakeIOWithReply conn "* OK [PERMANENTFLAGS (\\Answered \\Flagged\
    \ \\Draft \\Deleted \\Seen $NotPhishing $Phishing NonJunk \\*)]\
    \ Flags permitted." "OK k" $ sendCommand conn "test"
  forUntagged res isPermanentFlags $ \(PermanentFlags flags) -> do
    length flags @?= 9
    (isJust $ L.find isFAny flags) @?= True
    (isJust $ L.find isFDeleted flags) @?= True

    let otherFlags = map (\(FOther t) -> t) $ filter isFOther flags
    (isJust $ L.find (=="NonJunk") otherFlags) @?= True
    (isJust $ L.find (=="$Phishing") otherFlags) @?= True

newtype TestFlagList = TestFlagList B.ByteString deriving (Eq, Show)
newtype Atom = Atom B.ByteString deriving (Eq, Show)
newtype AtomChar = AtomChar Char deriving (Eq, Show)

atomSpecials :: String
atomSpecials = "(){ %*\\\n\r]\0"
availableFlags = ["Answered", "Flagged", "Deleted", "Seen", "Draft"]

instance Arbitrary AtomChar where
  arbitrary = do
    ch :: Char <- chr <$> choose (0,127)
    if L.any (\c -> c == ch) atomSpecials
      then arbitrary
      else return $ AtomChar ch

instance Arbitrary Atom where
  arbitrary = do
    howManyLetters :: Int <- choose (1,25)
    chars <- map (\(AtomChar c) -> c) `liftM` mapM (\_ -> arbitrary) [1..howManyLetters]
    return . Atom $ B.pack chars

instance Arbitrary TestFlagList where
  arbitrary = do
    howManyFlags :: Int <- choose (1,25)
    flags <- map (\(Atom a) -> B.append "\\" a) `liftM` mapM (\_ -> arbitrary) [1..howManyFlags]
    return . TestFlagList . B.concat $ ["(", (B.intercalate " " flags), ")"]

newtype AddressPart = AddressPart B.ByteString
instance Arbitrary AddressPart where
  arbitrary = do
    isNil :: Bool <- choose (False, True)
    case isNil of
      True -> return $ AddressPart "NIL"
      False -> do
        Atom atom <- arbitrary
        return . AddressPart $ B.concat ["\"", atom, "\""]

newtype MockAddress = MockAddress B.ByteString deriving (Eq, Show)
instance Arbitrary MockAddress where
  arbitrary = do
    labels <- mapM (\_ -> arbitrary >>= \(AddressPart label) -> return label) [1..4]
    return . MockAddress $ B.concat ["(", B.intercalate " " labels, ")"]

unparseEmailAddr :: EmailAddress -> B.ByteString
unparseEmailAddr (EmailAddress label route uname domain) =
  B.concat ["(", maybeToString label, maybeToString route,
            maybeToString uname, maybeToString domain, ")"]
    where maybeToString val = case val of
                                Nothing -> "NIL"
                                Just value -> B.concat ["\"", T.encodeUtf8 value, "\""]

unparseFlags :: [Flag] -> B.ByteString
unparseFlags parsedFlags = B.concat $ ["(", (B.intercalate " " unparsedFlags), ")"]
  where unparsedFlags = map (unparseFlag) parsedFlags
        unparseFlag FSeen = "\\Seen"
        unparseFlag FAnswered = "\\Answered"
        unparseFlag FFlagged = "\\Flagged"
        unparseFlag FDeleted = "\\Deleted"
        unparseFlag FDraft = "\\Draft"
        unparseFlag FRecent = "\\Recent"
        unparseFlag FAny = "\\*"
        unparseFlag (FOther f) = T.encodeUtf8 $ T.concat ["\\", f]

testEmailParsing :: MockAddress -> Bool
testEmailParsing (MockAddress addr) = case parsedAndUnparsed of
    Left _ -> False
    Right val -> val == addr
  where parsedAndUnparsed = A.parseOnly Utils.parseEmail addr >>= Right . unparseEmailAddr

testFlagParsing :: TestFlagList -> Bool
testFlagParsing (TestFlagList flagList) = if isLeft parsedAndUnparsed
    then False
    else fromRight' parsedAndUnparsed == flagList
  where parsedAndUnparsed = A.parseOnly U.parseFlagList flagList >>= Right . unparseFlags

tests :: TestTree
tests = testGroup "Network.IMAP" [
    testCase "Login Failure" testLoginFailure
  , testCase "Login Success" testLoginSuccess
  , testCase "Check flags reply" testFlags
  , testCase "Check EXISTS" testExists
  , testCase "Check RECENT" testRecent
  , testCase "Check UNSEEN" testUnseen
  , testCase "Check SEARCH 1" testSearch1
  , testCase "Check SEARCH 2" testSearch2
  , testCase "Check UIDNEXT" testUIDNext
  , testCase "Check PERMANENTFLAGS" testPermFlags
  , testProperty "Check flag parsing" testFlagParsing
  ]
