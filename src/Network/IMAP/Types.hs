module Network.IMAP.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.STM.RollingQueue as RQ

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TQueue (TQueue)
import Network.Connection (Connection, connectionPut, connectionGetChunk')
import ListT (ListT)
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (liftIO)
import qualified Debug.Trace as DT

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

data ResultState = OK | NO | BAD deriving (Show, Eq)

data Flag = FSeen
          | FAnswered
          | FFlagged
          | FDeleted
          | FDraft
          | FRecent
          | FAny
          | FOther T.Text
  deriving (Show, Eq)

data TaggedResult = TaggedResult {
                      commandId :: CommandId,
                      resultState :: !ResultState,
                      resultRest :: BSC.ByteString
                    } deriving (Show, Eq)

data UntaggedResult = Flags [Flag]
                    | Exists Int
                    | Recent Int
                    | Unseen Int
                    | PermanentFlags [Flag]
                    | UIDNext Int
                    deriving (Show, Eq)

data CommandResult = Tagged TaggedResult | Untagged UntaggedResult
  deriving (Show, Eq)

class Monad m => Universe m where
  connectionPut' :: Connection -> BSC.ByteString -> m ()
  connectionGetChunk'' :: Connection -> (BSC.ByteString -> (a, BSC.ByteString)) -> m a

instance Universe IO where
  connectionPut' = connectionPut
  connectionGetChunk'' = connectionGetChunk'

instance Universe (ListT IO) where
  connectionPut' c d = liftIO $ connectionPut c d
  connectionGetChunk'' c cont = liftIO $ connectionGetChunk' c cont
