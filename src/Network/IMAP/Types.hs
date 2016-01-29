module Network.IMAP.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TQueue (TQueue)
import Network.Connection (Connection)
import qualified Debug.Trace as DT
import System.IO.Unsafe (unsafePerformIO)
import Network.Connection

type ErrorMessage = T.Text
type CommandId = BSC.ByteString

data ConnectionState = Connected | Authenticated | Selected T.Text
data IMAPConnection = IMAPConnection {
  connectionState :: !ConnectionState, --Unused, will have the current state in a TVar
  untaggedQueue :: RQ.RollingQueue UntaggedResult,
  serverWatcherThread :: Maybe ThreadId,
  imapState :: IMAPState
}

data IMAPState = IMAPState {
  rawConnection :: !Connection,
  responseRequests :: TQueue ResponseRequest
}

data ResponseRequest = ResponseRequest {
  responseQueue :: TQueue CommandResult,
  respRequestId :: CommandId
} deriving (Eq)

data ResultState = OK | NO | BAD deriving (Show)

data Flag = FSeen
          | FAnswered
          | FFlagged
          | FDeleted
          | FDraft
          | FRecent
          | FAny
          | FOther T.Text
  deriving (Show)

data TaggedResult = TaggedResult {
                      commandId :: CommandId,
                      resultState :: !ResultState,
                      resultRest :: BSC.ByteString
                    } deriving (Show)

data UntaggedResult = Flags [Flag]
                    | Exists Int
                    | Recent Int
                    | Unseen Int
                    | PermanentFlags [Flag]
                    | UIDNext Int
                    deriving (Show)

data CommandResult = Tagged TaggedResult | Untagged UntaggedResult
  deriving (Show)

newtype Bytes a = Bytes BSC.ByteString

class Monad m => OverloadableConnection m where
  bytesWritten :: TVar (Bytes (m ()))
  bytesToWrite :: TMVar (Bytes (m ()))

  connectionPut :: Connection -> BSC.ByteString -> m ()
  connectionGetChunk' :: Connection -> (BSC.ByteString -> (a, BSC.ByteString)) -> m a

instance OverloadableConnection IO where
  connectionPut = DT.trace "using here" $ Network.Connection.connectionPut
  connectionGetChunk' = DT.trace "chunk" $ Network.Connection.connectionGetChunk'
