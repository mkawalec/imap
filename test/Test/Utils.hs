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
import Data.Maybe (fromJust, isJust)
import Control.Monad (when)
import ListT (toList, ListT(..))

data FakeState = FS {
  bytesWritten :: TVar BS.ByteString,
  bytesToRead :: TVar BS.ByteString,
  reactToInput :: (BS.ByteString -> BS.ByteString)
}

def :: IO FakeState
def = do
  bytesWritten <- newTVarIO BS.empty
  bytesToRead <- newTVarIO BS.empty

  return FS {
    bytesWritten = bytesWritten,
    bytesToRead = bytesToRead,
    reactToInput = id
  }

testConnectionPut :: c -> BS.ByteString -> S.StateT FakeState IO ()
testConnectionPut _ input = do
  st <- S.get
  let toRead = (reactToInput st) input

  liftIO . atomically $ do
    alreadyWritten <- readTVar $ bytesWritten st
    let newWritten = BS.append alreadyWritten input
    writeTVar (bytesToRead st) toRead
    writeTVar (bytesWritten st) newWritten

testConnectionGetChunk :: Connection -> (BS.ByteString -> (a, BS.ByteString)) -> S.StateT FakeState IO a
testConnectionGetChunk c proc = do
  st <- S.get
  toRead <- liftIO . atomically . readTVar . bytesToRead $ st
  if (BS.length toRead) == 0
    then (lift $ threadDelay 10000) >> testConnectionGetChunk c proc
    else do
      let (result, left) = proc toRead
      liftIO . atomically $ writeTVar (bytesToRead st) left
      return result


instance {-# OVERLAPPING #-} Universe (S.StateT FakeState IO) where
  connectionPut' = testConnectionPut
  connectionGetChunk'' = testConnectionGetChunk

instance {-# OVERLAPPING #-} Universe (ListT (S.StateT FakeState IO)) where
  connectionPut' c bs = lift $ testConnectionPut c bs
  connectionGetChunk'' c cont = lift $ testConnectionGetChunk c cont

instance HasFork (S.StateT FakeState IO) where
  fork em = S.StateT $ \s -> do
    defState <- def
    threadId <- forkIO $ (S.runStateT em s >>= return . fst)
    return (threadId, defState)

runFakeIO :: FakeState -> StateT FakeState IO a -> IO (a, FakeState)
runFakeIO = flip S.runStateT

withWatcher :: IMAPConnection -> StateT FakeState IO a -> StateT FakeState IO [a]
withWatcher conn action = toList $ withWatcher' conn action

withWatcher' :: IMAPConnection -> StateT FakeState IO a -> ListT (StateT FakeState IO) a
withWatcher' conn action = do
  -- Kill the old watcher
  st <- lift $ S.get
  when (isJust . serverWatcherThread $ conn) $ do
    lift . killThread . fromJust . serverWatcherThread $ conn
  watcherThreadId <- lift . fork $ requestWatcher conn []

  (res, newState) <- lift . lift $ S.runStateT action st
  lift $ killThread watcherThreadId
  lift $ S.put newState
  return res



respond :: T.Text -> BS.ByteString -> BS.ByteString
respond response input = encodeUtf8 $ T.concat [commandId, " ", response, "\r\n"]
  where commandId = head . T.splitOn " " $ decodeUtf8 input
