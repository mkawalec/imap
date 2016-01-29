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

import Control.Monad.Trans.Identity

instance Monad (IdentityT IO) where
  (>>=) = (>>=)

instance OverloadableConnection (IdentityT IO) where
  bytesWritten = unsafePerformIO . newTVarIO . Bytes $ BS.empty
  bytesToWrite = unsafePerformIO $ newEmptyTMVarIO

  connectionPut _ input = do
    DT.trace ("writing " ++ (show input)) $ return ()
    lift . atomically $ writeTVar (bytesWritten :: TVar (Bytes (IO ()))) (Bytes input)
  connectionGetChunk' _ proc = lift . atomically $ do
    Bytes bytes <- takeTMVar (bytesToWrite :: TMVar (Bytes (IO ())))
    let (result, left) = proc bytes
    putTMVar (bytesToWrite :: TMVar (Bytes (IO ()))) $ Bytes left
    return result

testLogin :: IO ()
testLogin = do
  runIdentityT $ do
    connection <- liftIO connectServer
    DT.trace "before" $ return ()
    res <- login connection "a" "b"
    Bytes written <- lift . atomically $ readTVar (bytesWritten :: TVar (Bytes (IO ())))
    DT.traceShow written $ return ()

tests :: TestTree
tests = testGroup "Network.IMAP" [testCase "testLogin" testLogin]
