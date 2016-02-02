module Network.IMAP.Tests (tests) where

import Network.IMAP
import Network.IMAP.Types
import Test.Utils

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), assertFailure)
import qualified Debug.Trace as DT
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)

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

testLoginFailure conn = do
  (res, _) <- runFakeIOWithReply conn "" "NO [ALERT] Invalid credentials (Failure)" $ do
    login conn "a" "b"

  lastIsTagged res $ \r -> resultState r @?= NO

testLoginSuccess conn = do
  (res, _) <- runFakeIOWithReply conn
    "* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN \
    \X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE ENABLE MOVE CONDSTORE ESEARCH \
    \UTF8=ACCEPT LIST-EXTENDED LIST-STATUS" "OK a@b authenticated (Success)" $
      login conn "a" "b"

  lastIsTagged res $ \r -> resultState r @?= OK

testFlags conn = do
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

testExists conn = do
  (res, _) <- runFakeIOWithReply conn "* 67 EXISTS" "OK k" $ sendCommand conn "test"
  forUntagged res isExists $ \(Exists count) -> count @?= 67

testRecent conn = do
  (res, _) <- runFakeIOWithReply conn "* 15 RECENT" "OK k" $ sendCommand conn "test"
  forUntagged res isRecent $ \(Recent count) -> count @?= 15

testUnseen conn = do
  (res, _) <- runFakeIOWithReply conn "* OK [UNSEEN 15]" "OK k" $ sendCommand conn "test"
  forUntagged res isUnseen $ \(Unseen count) -> count @?= 15

testUIDNext conn = do
  (res, _) <- runFakeIOWithReply conn "* OK [UIDNEXT 923] Predicted next UID." "OK k" $
    sendCommand conn "test"

  forUntagged res isUIDNext $ \(UIDNext nextUID) -> nextUID @?= 923

testPermFlags conn = do
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


tests :: IMAPConnection -> TestTree
tests conn = testGroup "Network.IMAP" [
  testCase "Login Failure" (testLoginFailure conn),
  testCase "Login Success" (testLoginSuccess conn),
  testCase "Check flags reply" (testFlags conn),
  testCase "Check EXISTS" (testExists conn),
  testCase "Check RECENT" (testRecent conn),
  testCase "Check UNSEEN" (testUnseen conn),
  testCase "Check UIDNEXT" (testUIDNext conn),
  testCase "Check PERMANENTFLAGS" (testPermFlags conn)
  ]
