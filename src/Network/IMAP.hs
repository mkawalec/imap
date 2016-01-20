module Network.IMAP where

import Network.Connection
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT
import Data.Maybe (isJust, fromJust)

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8
import qualified Data.List as L

import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM

import Data.Either (isRight)
import Data.Either.Combinators (fromRight', mapLeft)
import Control.Concurrent (forkIO, ThreadId, myThreadId)

import Network.IMAP.Types
import Network.IMAP.Parsers
import Network.IMAP.RequestWatcher
import Network.IMAP.Utils


connectServer :: IO IMAPConnection
connectServer = do
  context <- initConnectionContext
  let params = ConnectionParams "imap.gmail.com" 993 Nothing Nothing
  let tlsSettings = TLSSettingsSimple False False False

  connection <- connectTo context params
  connectionSetSecure context connection tlsSettings

  untaggedRespsQueue <- RQ.newIO 20
  repliesMap <- newTVarIO M.empty
  responseRequestsQueue <- newTQueueIO

  let state = IMAPState {
    rawConnection = connection,
    commandReplies =  repliesMap,
    responseRequests = responseRequestsQueue
  }

  let conn = IMAPConnection {
    connectionState = Connected,
    serverWatcherThread = Nothing,
    untaggedQueue = untaggedRespsQueue,
    imapState = state
  }

  watcherThreadId <- forkIO $ requestWatcher conn []

  return conn {
    serverWatcherThread = Just watcherThreadId
  }

sendCommand :: IMAPConnection -> BSC.ByteString -> IO RequestResponse
sendCommand conn command = do
  let state = imapState conn
  requestId <- genRequestId
  let commandLine = BSC.concat [requestId, " ", command, "\r\n"]

  connectionPut (rawConnection state) commandLine
  responseWrapper <- atomically $ newEmptyTMVar

  let responseRequest = ResponseRequest responseWrapper requestId
  atomically $ writeTQueue (responseRequests state) responseRequest
  atomically $ takeTMVar responseWrapper

login :: IMAPConnection -> T.Text -> T.Text -> IO RequestResponse
login conn username password = sendCommand conn . encodeUtf8 $
  T.intercalate " " ["LOGIN", escapeText username, escapeText password]

escapeText :: T.Text -> T.Text
escapeText t = T.replace "{" "\\{" $
             T.replace "}" "\\}" $
             T.replace "\"" "\\\"" $
             T.replace "\\" "\\\\" t
