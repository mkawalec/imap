module Network.IMAP.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TVar (TVar)
import Data.DeriveTH

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TQueue (TQueue)
import Network.Connection (Connection, connectionPut, connectionGetChunk')
import ListT (ListT)
import Control.Monad.IO.Class (liftIO)

type ErrorMessage = T.Text
type CommandId = BSC.ByteString

data ConnectionState = UndefinedState
                     | Connected
                     | Disconnected
                     deriving (Show)

data IMAPConnection = IMAPConnection {
  connectionState :: TVar ConnectionState,
  untaggedQueue :: RQ.RollingQueue UntaggedResult,
  serverWatcherThread :: TVar (Maybe ThreadId),
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
  deriving (Show, Eq, Ord)

data Capability = CIMAP4
                | CUnselect
                | CIdle
                | CNamespace
                | CQuota
                | CId
                | CExperimental T.Text
                | CChildren
                | CUIDPlus
                | CCompress T.Text
                | CEnable
                | CMove
                | CCondstore
                | CEsearch
                | CUtf8 T.Text
                | CAuth T.Text
                | CListExtended
                | CListStatus
                | CAppendLimit Int
                | COther T.Text (Maybe T.Text)
                deriving (Show, Eq, Ord)


data TaggedResult = TaggedResult {
                      commandId :: CommandId,
                      resultState :: !ResultState,
                      resultRest :: BSC.ByteString
                    } deriving (Show, Eq)

data NameAttribute = Noinferiors
                   | Noselect
                   | Marked
                   | Unmarked
                   | HasNoChildren
                   | OtherNameAttr T.Text
                   deriving (Show, Eq, Ord)

data UntaggedResult = Flags [Flag]
                    | Exists Int
                    | Bye
                    | HighestModSeq Int
                    | Recent Int
                    | Messages Int
                    | Unseen Int
                    | PermanentFlags [Flag]
                    | UIDNext Int
                    | UIDValidity Int
                    | OKResult T.Text
                    | Capabilities [Capability]
                    | ListR [NameAttribute] T.Text T.Text
                    | StatusR T.Text [UntaggedResult]
                    deriving (Show, Eq, Ord)

data CommandResult = Tagged TaggedResult | Untagged UntaggedResult
  deriving (Show, Eq)

type SimpleResult = Either ErrorMessage [UntaggedResult]

$(derive makeIs ''Flag)
$(derive makeIs ''UntaggedResult)
$(derive makeIs ''CommandResult)
$(derive makeIs ''ConnectionState)

class Monad m => Universe m where
  connectionPut' :: Connection -> BSC.ByteString -> m ()
  connectionGetChunk'' :: Connection -> (BSC.ByteString -> (a, BSC.ByteString)) -> m a

instance Universe IO where
  connectionPut' = connectionPut
  connectionGetChunk'' = connectionGetChunk'

instance Universe (ListT IO) where
  connectionPut' c d = liftIO $ connectionPut c d
  connectionGetChunk'' c cont = liftIO $ connectionGetChunk' c cont
