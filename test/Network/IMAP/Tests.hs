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
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Debug.Trace as DT


newtype LolIO a = IO a
  deriving (Functor, Monad, Applicative)


instance MonadIO LolIO where
  liftIO val = return $ unsafePerformIO val

instance OverloadableConnection LolIO where
  bytesWritten = unsafePerformIO . newTVarIO . Bytes $ BS.empty
  bytesToWrite = unsafePerformIO $ newEmptyTMVarIO

  connectionPut _ input = do
    liftIO . atomically $ writeTVar (bytesWritten :: TVar (Bytes (LolIO ()))) (Bytes input)
    DT.trace ("writing " ++ (show input)) $ return ()
  connectionGetChunk' _ proc = liftIO . atomically $ do
    Bytes bytes <- takeTMVar (bytesToWrite :: TMVar (Bytes (LolIO ())))
    let (result, left) = proc bytes
    putTMVar (bytesToWrite :: TMVar (Bytes (LolIO ()))) $ Bytes left
    return result

testLogin :: LolIO ()
testLogin = do
  connection <- connectServer
  DT.trace "amin" $ return ()
  res <- login connection "a" "b"
  Bytes written <- liftIO . atomically $ readTVar (bytesWritten :: TVar (Bytes (LolIO ())))
  DT.traceShow  $ return ()
  DT.traceShow written $ return ()

tests :: TestTree
tests = testGroup "Network.IMAP" [testCase "testLogin" testLogin]
