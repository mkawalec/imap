module Test.Utils where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Network.Connection
import Control.Monad.Trans.Class

import qualified Data.ByteString as BS
import Control.Monad.State.Strict as S

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as T
import Network.IMAP.Types
import ListT (ListT)
import Control.Concurrent (threadDelay)
import Network.IMAP.RequestWatcher (requestWatcher)
import Network.IMAP (connectServer)
import Control.Concurrent.MonadIO (fork, HasFork, fork, killThread)
import Control.Concurrent (forkIO)
import Data.Maybe (fromJust)
import ListT (toList)

data FakeState = FS {
  bytesWritten :: TVar BS.ByteString,
  bytesToRead :: TVar BS.ByteString,
  reactToInput :: (BS.ByteString -> BS.ByteString)
}

def :: IO FakeState
def = do
  written <- newTVarIO BS.empty
  toRead <- newTVarIO BS.empty

  return FS {
    bytesWritten = written,
    bytesToRead = toRead,
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
    then (lift $ threadDelay 1000) >> testConnectionGetChunk c proc
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

runFakeIOWithReply :: IMAPConnection -> T.Text -> T.Text -> ListT (StateT FakeState IO) a -> IO ([a], FakeState)
runFakeIOWithReply conn prefix reply action = do
  defState <- def
  runFakeIO defState {reactToInput = respond prefix reply} $ withWatcher conn $ action

getConn :: IO IMAPConnection
getConn = do
  let tlsSettings = Just $ TLSSettingsSimple False False False
  let params = ConnectionParams "imap.gmail.com" 993 tlsSettings Nothing

  conn <- connectServer params 
  threadId <- atomically . readTVar $ serverWatcherThread conn
  killThread . fromJust $ threadId

  atomically $ writeTVar (serverWatcherThread conn) Nothing
  return conn

runFakeIO :: FakeState -> StateT FakeState IO a -> IO (a, FakeState)
runFakeIO = flip S.runStateT

respond :: T.Text -> T.Text -> BS.ByteString -> BS.ByteString
respond prefix response input =
    encodeUtf8 $ T.concat [prefix, "\r\n", commandId, " ", response, "\r\n"]
  where commandId = head . T.splitOn " " $ decodeUtf8 input

withWatcher :: IMAPConnection -> ListT (StateT FakeState IO) a -> StateT FakeState IO [a]
withWatcher conn action = withWatcher' conn action

withWatcher' :: IMAPConnection -> ListT (StateT FakeState IO) a -> StateT FakeState IO [a]
withWatcher' conn action = do
  st <- S.get
  watcherThreadId <- fork $ requestWatcher conn

  (res, _) <- lift $ S.runStateT (toList action) st
  lift $ killThread watcherThreadId
  return res
