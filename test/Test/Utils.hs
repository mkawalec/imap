module Test.Utils where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Network.Connection
import Network.Socket
import Network.Socket.ByteString
import Control.Monad.Trans.Class
import GHC.Conc (threadStatus)

import qualified Data.ByteString as BS
import Control.Monad.State.Strict as S

import Control.Concurrent.MVar
import Control.Exception (catch, AsyncException, SomeException, throw)

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
import Control.Monad (void)
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
  toRead <- liftIO . atomically $ do
    bytes <- readTVar . bytesToRead $ st
    if (BS.length bytes) == 0 then retry else return bytes

  lift $ threadDelay 10000
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

data WaitFlag = WaitFlag

mockServer :: Integer -> MVar WaitFlag -> MVar WaitFlag -> IO ()
mockServer port waitMVar killMVar = (do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
  recvSocket <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption recvSocket ReuseAddr 1
  setSocketOption recvSocket ReusePort 1
  bind recvSocket $ addrAddress addr
  listen recvSocket 1
  putMVar waitMVar WaitFlag
  (acceptedSocket, _) <- accept recvSocket
  void $ Network.Socket.ByteString.recv acceptedSocket 4096
  ) `catch` (
    \e -> return (e :: AsyncException) >> void (tryPutMVar killMVar WaitFlag))

withMockServer :: IO a -> IO a
withMockServer action = do
  waitMVar <- newEmptyMVar
  killMVar <- newEmptyMVar
  mockServerId <- forkIO $ mockServer 8023 waitMVar killMVar
  readMVar waitMVar
  result <- action
  killThread mockServerId
  readMVar killMVar
  status <- threadStatus mockServerId

  return result

getConn :: IO IMAPConnection
getConn = withMockServer $ do
  let tlsSettings = Just $ TLSSettingsSimple False False False
  let params = ConnectionParams "127.0.0.1" 8023 Nothing Nothing

  conn <- connectServer params Nothing
  let state = imapState conn
  threadId <- atomically . readTVar $ serverWatcherThread state
  killThread . fromJust $ threadId

  atomically $ writeTVar (serverWatcherThread state) Nothing
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
