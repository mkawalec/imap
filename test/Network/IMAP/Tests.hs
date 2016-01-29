{-# LANGUAGE OverlappingInstances #-}
module Network.IMAP.Tests (tests) where

import Network.IMAP
import Network.IMAP.Types

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))

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

import Control.Monad.Trans.Identity

data FakeState = FS {
  bytesWritten :: BS.ByteString,
  bytesToRead :: BS.ByteString
} deriving (Show)

def :: FakeState
def = FS {bytesWritten = BS.empty, bytesToRead = "error replu"}

instance OverloadableConnection (S.StateT FakeState IO) where
  connectionPut' _ input = do
    DT.trace ("writing " ++ (show input)) $ return ()
    st <- S.get
    S.put st { bytesWritten = input }
  connectionGetChunk'' _ proc = do
    DT.trace "reading" $ return ()
    st <- S.get
    let (result, left) = proc . bytesToRead $ st
    S.put st { bytesToRead = left }
    return result

runFakeIO :: FakeState -> StateT FakeState IO a -> IO (a, FakeState)
runFakeIO = flip runStateT

testLogin :: IO ()
testLogin = do
  connection <- connectServer
  DT.traceShow "connected" $ return ()
  res <- runFakeIO def $ DT.trace "before do" $ do
    DT.trace "before" $ return ()
    res <- lift $ login connection "a" "b"
    st <- S.get
    DT.trace ("got bytes " ++ show (st)) $ return ()
  DT.traceShow "afterio" $ return ()
  return . fst $ res

tests :: TestTree
tests = testGroup "Network.IMAP" [testCase "testLogin" testLogin]
