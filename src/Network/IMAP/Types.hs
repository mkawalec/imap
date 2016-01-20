module Network.IMAP.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.STM.RollingQueue as RQ

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TMVar (TMVar)
import Network.Connection (Connection)
import qualified Data.Map.Strict as M

type ErrorMessage = T.Text
type RequestId = BSC.ByteString

data ConnectionState = Connected | Authenticated | Selected T.Text
data IMAPConnection = IMAPConnection {
  connectionState :: !ConnectionState, --Unused, will have the current state in a TVar
  untaggedQueue :: RQ.RollingQueue UntaggedResult,
  serverWatcherThread :: Maybe ThreadId,
  imapState :: IMAPState
}

data IMAPState = IMAPState {
  rawConnection :: !Connection,
  commandReplies :: TVar (M.Map RequestId RequestResponse),
  responseRequests :: TQueue ResponseRequest
}

data ResponseRequest = ResponseRequest {
  requestResponse :: TMVar RequestResponse,
  respRequestId :: RequestId
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
                      requestId :: RequestId,
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

data RequestResponse = RequestResponse {
  untaggedResults :: [UntaggedResult],
  taggedResult :: Maybe TaggedResult
} deriving (Show)
