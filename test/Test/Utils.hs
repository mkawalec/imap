module Test.Utils where

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
import Network.IMAP.Types
import Control.Concurrent.STM.TVar
import ListT (ListT)
import Control.Concurrent (threadDelay, yield)
import Network.IMAP.RequestWatcher (requestWatcher)
import Control.Concurrent.MonadIO (fork, HasFork, fork, killThread)
import Control.Concurrent (forkIO)
import Data.Maybe (fromJust)

data FakeState = FS {
  bytesWritten :: !BS.ByteString,
  bytesToRead :: TVar BS.ByteString,
  reactToInput :: (BS.ByteString -> BS.ByteString)
}

def :: FakeState
def = FS {
  bytesWritten = BS.empty,
  bytesToRead = unsafePerformIO . newTVarIO $ BS.empty,
  reactToInput = id}

testConnectionPut :: c -> BS.ByteString -> S.StateT FakeState IO ()
testConnectionPut _ input = do
  st <- S.get
  let toRead = (reactToInput st) input
  DT.trace ("putting " ++ (show toRead)) $ return ()
  liftIO . atomically $ writeTVar (bytesToRead st) toRead
  S.put st {
    bytesWritten = BS.append (bytesWritten st) input
  }

testConnectionGetChunk :: Connection -> (BS.ByteString -> (a, BS.ByteString)) -> S.StateT FakeState IO a
testConnectionGetChunk c proc = do
  st <- S.get
  toRead <- liftIO . atomically . readTVar . bytesToRead $ st
  if (BS.length toRead) == 0
    then (lift $ threadDelay 1000000) >> testConnectionGetChunk c proc
    else do
      let (result, left) = proc toRead
      liftIO . atomically $ writeTVar (bytesToRead st) left
      return result


instance {-# OVERLAPPING #-} Universe (S.StateT FakeState IO) where
  connectionPut' = testConnectionPut
  connectionGetChunk'' = testConnectionGetChunk

instance HasFork (S.StateT FakeState IO) where
  fork em = S.StateT $ \s -> do
    threadId <- forkIO $ (S.runStateT em s >>= return . fst)
    return (threadId, def)

runFakeIO :: FakeState -> StateT FakeState IO a -> IO (a, FakeState)
runFakeIO = flip runStateT

withWatcher :: IMAPConnection -> StateT FakeState IO a -> StateT FakeState IO a
withWatcher conn action = do
  -- Kill the old watcher
  st <- S.get
  killThread . fromJust . serverWatcherThread $ conn
  watcherThreadId <- fork $ requestWatcher conn []
  let newConn = conn {
    serverWatcherThread = Just watcherThreadId
  }

  DT.traceShow (reactToInput st "abc efg") $ return ()
  (res, newState) <- lift $ runStateT action st
  killThread watcherThreadId
  return res



respond :: T.Text -> BS.ByteString -> BS.ByteString
respond response input = DT.trace "respond called" $ encodeUtf8 $ T.concat [commandId, " ", response, "\r\n"]
  where commandId = head . T.splitOn " " $ decodeUtf8 input
