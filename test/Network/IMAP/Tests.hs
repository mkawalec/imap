module Network.IMAP.Tests (tests) where

import Network.IMAP

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

import qualified Data.ByteString as BS
import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO)
import qualified Debug.Trace as DT


instance OverloadableConnection IO where
  bytesWritten = unsafePerformIO . newTVarIO . Bytes $ BS.empty
  bytesToWrite = unsafePerformIO $ newEmptyTMVarIO

  connectionPut _ input = do
    atomically $ writeTVar (bytesWritten :: TVar (Bytes (IO ()))) (Bytes input)
    DT.trace ("writing " ++ (show input)) $ return ()
  connectionGetChunk' _ proc = atomically $ do
    Bytes bytes <- takeTMVar (bytesToWrite :: TMVar (Bytes (IO ())))
    let (result, left) = proc bytes
    putTMVar (bytesToWrite :: TMVar (Bytes (IO ()))) $ Bytes left
    return result

testLogin :: Assertion
testLogin = do
  connection <- connectServer
  DT.trace "amin" $ return ()
  res <- login connection "a" "b"
  Bytes written <- atomically $ readTVar (bytesWritten :: TVar (Bytes (IO ())))
  DT.traceShow  $ return ()
  DT.traceShow written $ return ()

tests :: TestTree
tests = testGroup "Network.IMAP" [testCase "testLogin" testLogin]
