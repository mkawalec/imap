module Network.IMAP.Tests (tests) where

import Network.IMAP
import Network.IMAP.Types

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=), assertFailure)

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Network.Connection
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Class
import Control.Monad

import qualified Data.ByteString as BS
import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Debug.Trace as DT
import Control.Monad.State.Strict as S

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as T
import ListT (toList, ListT)

import Control.Monad.Trans.Identity
import Test.Utils

testLoginFailure :: Assertion
testLoginFailure = do
  connection <- connectServer
  defState <- def
  (res, state) <- runFakeIO defState {reactToInput = respond "NO [ALERT] Invalid credentials (Failure)"} $
    withWatcher connection $ login connection "a" "b"

  written <- atomically . readTVar . bytesWritten $ state

  case last res of
    Untagged _ -> assertFailure "The last item should be Tagged"
    Tagged r -> resultState r @?= NO
  return ()

tests :: TestTree
tests = testGroup "Network.IMAP" [testCase "Login Failure" testLoginFailure]
