module Network.IMAP.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.STM.RollingQueue as RQ
import Control.Concurrent.STM.TVar (TVar)
-- import Data.DeriveTH

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TQueue (TQueue)
import Network.Connection (Connection, ConnectionContext,
  connectionPut, connectionGetChunk')
import ListT (ListT)
import Control.Monad.IO.Class (liftIO)
import qualified Pipes as P

-- |A type alias used for an error message
type ErrorMessage = T.Text
-- |Each command sent to the server is identified by a random id.
--  this alias helps noticing where this happens
type CommandId = BSC.ByteString

-- |Connection with the server can be in one of these states
data ConnectionState = UndefinedState
                     | Connected
                     | Disconnected
                     deriving (Show)

isUndefinedState, isConnected, isDisconnected :: ConnectionState -> Bool

isUndefinedState UndefinedState = True; isUndefinedState _ = False
isConnected Connected = True; isConnected _ = False
isDisconnected Disconnected = True; isDisconnected _ = False

data IMAPConnection = IMAPConnection {
  -- |The current connection state
  connectionState :: TVar ConnectionState,
  -- |Contains commands sent by the server which we didn't expect.
  --  Probably message and mailbox state updates
  untaggedQueue :: RQ.RollingQueue UntaggedResult,
  -- |Internal state of the library
  imapState :: IMAPState
}

data IMAPState = IMAPState {
  -- |The actual connection with the server from
  --  Network.Connection. Only use if you know what you're doing
  rawConnection :: !Connection,
  -- |Context from Network.Connection
  connectionContext :: ConnectionContext,
  -- |Contains requests for response that weren't yet read by the watcher thread.
  responseRequests :: TQueue ResponseRequest,
  -- |Id of the thread the watcher executes on
  serverWatcherThread :: TVar (Maybe ThreadId),
  -- |All the unfulfilled requests the watcher thread knows about
  outstandingReqs :: TVar [ResponseRequest],
  -- |Configuration settings
  imapSettings :: IMAPSettings
}

type ParseResult = Either ErrorMessage CommandResult

data ResponseRequest = ResponseRequest {
  -- |Thread that posted the request should watch this
  --  queue for responses to the request.
  responseQueue :: TQueue CommandResult,
  -- |Id of the request, which is the same as the id sent to the server.
  respRequestId :: CommandId
} deriving (Eq)

data IMAPSettings = IMAPSettings {
  -- Number of seconds after which request timeouts
  imapTimeout :: Int,
  -- Length of a queue containing messages we weren't expecting
  untaggedQueueLength :: Int
}

data EmailAddress = EmailAddress {
  emailLabel :: Maybe T.Text,
  emailRoute :: Maybe T.Text,
  emailUsername :: Maybe T.Text,
  emailDomain :: Maybe T.Text
} deriving (Show, Eq)

data Flag = FSeen
          | FAnswered
          | FFlagged
          | FDeleted
          | FDraft
          | FRecent
          | FAny
          | FOther T.Text
  deriving (Show, Eq, Ord)

isFOther, isFAny, isFRecent, isFDraft, isFDeleted, isFFlagged, isFAnswered, isFSeen :: Flag -> Bool
isFOther (FOther _) = True; isFOther _ = False
isFAny FAny = True; isFAny _ = False
isFRecent FRecent = True; isFRecent _ = False
isFDraft FDraft = True; isFDraft _ = False
isFDeleted FDeleted = True; isFDeleted _ = False
isFFlagged FFlagged = True; isFFlagged _ = False
isFAnswered FAnswered = True; isFAnswered _ = False
isFSeen FSeen = True; isFSeen _ = False

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
                -- |First parameter is the name of a capability
                --  and the second can contain a value, if the capability
                --  is of the form `NAME=VALUE`
                | COther T.Text (Maybe T.Text)
                deriving (Show, Eq, Ord)


-- |Always the last result of the command, contains it's metadata
data TaggedResult = TaggedResult {
                      -- |Id of the command that completes
                      commandId :: CommandId,
                      -- |State returned by the server side
                      resultState :: !ResultState,
                      -- |Rest of the result, usually the human-readable part
                      resultRest :: T.Text
                    } deriving (Show, Eq)

-- |Tagged results can be in on of these three states
data ResultState = OK | NO | BAD deriving (Show, Eq)

-- |Untagged replies are the actual data returned in response to the commands.
data UntaggedResult = Flags [Flag] -- ^ A list of flags a mailbox has
                    | Exists Integer -- ^ How many messages exist in a mailbox
                    | Expunge Integer -- ^ Sequence id of a deleted message
                    | Bye -- ^ Returned by the server when it cleanly disconnects
                    | HighestModSeq Integer
                    | Recent Integer -- ^ Number of recent messages
                    | Messages Integer -- ^ Number of messages in a mailbox
                    | Unseen Integer -- ^ Number of unseen messages
                    | PermanentFlags [Flag]
                    | UID Integer -- ^ UID of a message
                    | MessageId Integer -- ^ A sequence id of a message
                    -- |UID that will be given to the next message added to this mailbox
                    | UIDNext Integer
                    -- |A triple of mailbox name, it's UIDValidity value and message UID
                    --  is always unique for a given message
                    | UIDValidity Integer
                    | OKResult T.Text -- ^ Result of an OK response
                    | NOResult T.Text -- ^ Result of a NO response
                    | BADResult T.Text -- ^ Result of a BAD response
                    | Capabilities [Capability] -- ^ What server advertises that it supports
                    -- |Response to the `LIST` command
                    | ListR {
                      flags :: [NameAttribute], -- ^ flags that a mailbox has
                      -- |Character sequence that marks a new level of hierarchy
                      --  in the inbox name (usually a slash)
                      hierarchyDelimiter :: T.Text,
                      -- |Name of the mailbox
                      inboxName :: T.Text
                    }
                    | Fetch [UntaggedResult] -- ^ Fetch response, contains many responses
                    -- |Status of a mailbox, will contain many different responses inside
                    | StatusR T.Text [UntaggedResult]
                    -- |A list of message IDs or UIDs fullfilling the search criterions
                    | Search [Integer]
                    -- |A parsed ENVELOPE reply, prefixed to avoid name clashes
                    | Envelope {
                      eDate :: Maybe T.Text,
                      eSubject :: Maybe T.Text,
                      eFrom :: Maybe [EmailAddress],
                      eSender :: Maybe [EmailAddress],
                      eReplyTo :: Maybe [EmailAddress],
                      eTo :: Maybe [EmailAddress],
                      eCC :: Maybe [EmailAddress],
                      eBCC :: Maybe [EmailAddress],
                      eInReplyTo :: Maybe T.Text,
                      eMessageId :: Maybe T.Text
                    }
                    | InternalDate T.Text
                    | Size Integer -- ^ Message size
                    | Unknown BSC.ByteString -- ^ An unsupported value
                    | Body BSC.ByteString -- ^ Message body, or headers
                    | BodyStructure BSC.ByteString -- ^ An unparsed bodystructure
                    | Extension BSC.ByteString ExtensionPayload -- ^ A format extension
                    deriving (Show, Eq)

isFlags, isExists, isExpunge, isBye, isHighestModSeq, isRecent, isMessages :: UntaggedResult -> Bool
isUnseen, isPermanentFlags, isUID, isMessageId, isUIDNext, isUIDValidity :: UntaggedResult -> Bool
isOKResult, isNOResult, isBADResult, isCapabilities, isListR, isFetch :: UntaggedResult -> Bool
isStatusR, isSearch, isEnvelope, isInternalDate, isSize, isUnknown :: UntaggedResult -> Bool
isBody, isBodyStructure, isExtension :: UntaggedResult -> Bool

isFlags (Flags _) = True; isFlags _ = False
isExists (Exists{}) = True; isExists _ = False
isExpunge (Expunge _) = True; isExpunge _ = False
isBye Bye = True; isBye _ = False
isHighestModSeq (HighestModSeq _) = True; isHighestModSeq _ = False
isRecent (Recent _) = True; isRecent _ = False
isMessages (Messages _) = True; isMessages _ = False
isUnseen (Unseen _) = True; isUnseen _ = False
isPermanentFlags (PermanentFlags _) = True; isPermanentFlags _ = False
isUID (UID _) = True; isUID _ = False
isMessageId (MessageId _) = True; isMessageId _ = False
isUIDNext (UIDNext _) = True; isUIDNext _ = False
isUIDValidity (UIDValidity _) = True; isUIDValidity _ = False
isOKResult (OKResult _) = True; isOKResult _ = False
isNOResult (NOResult _) = True; isNOResult _ = False
isBADResult (BADResult _) = True; isBADResult _ = False
isCapabilities (Capabilities _) = True; isCapabilities _ = False
isListR (ListR{}) = True; isListR _ = False
isFetch (Fetch{}) = True; isFetch _ = False
isStatusR (StatusR{}) = True; isStatusR _ = False
isSearch (Search{}) = True; isSearch _ = False
isEnvelope (Envelope{}) = True; isEnvelope _ = False
isInternalDate (InternalDate{}) = True; isInternalDate _ = False
isSize (Size{}) = True; isSize _ = False
isUnknown (Unknown{}) = True; isUnknown _ = False
isBody  (Body{}) = True; isBody _ = False
isBodyStructure  (BodyStructure{}) = True; isBodyStructure _ = False
isExtension  (Extension{}) = True; isExtension _ = False

data ExtensionPayload = ExtInt Integer | ExtLabels [BSC.ByteString]
  deriving (Show, Eq)

data NameAttribute = Noinferiors
                   | Noselect
                   | Marked
                   | Unmarked
                   | HasNoChildren
                   | OtherNameAttr T.Text
                   deriving (Show, Eq, Ord)

-- |Command result consits of a sequence of untagged results followed
--  by a single tagged result that specifies if the command overall succeeded.
--  This is a sum type to bind those two types together
data CommandResult = Tagged TaggedResult | Untagged UntaggedResult
  deriving (Show, Eq)

isTagged, isUntagged :: CommandResult -> Bool
isTagged (Tagged{}) = True; isTagged _ = False
isUntagged (Untagged{}) = True; isUntagged _ = False



-- |If you don't care about streaming you will get results in this simplified
--  data type, in which the ErrorMessage comes from TaggedResult if it failed.
type SimpleResult = Either ErrorMessage [UntaggedResult]

-- |Every function that communicates with the outside world should run
--  in the Universe monad, which provides an ability to use mocks when testing
class Monad m => Universe m where
  connectionPut' :: Connection -> BSC.ByteString -> m ()
  connectionGetChunk'' :: Connection -> (BSC.ByteString -> (a, BSC.ByteString)) -> m a

instance Universe IO where
  connectionPut' = connectionPut
  connectionGetChunk'' = connectionGetChunk'

instance Universe (ListT IO) where
  connectionPut' c d = liftIO $ connectionPut c d
  connectionGetChunk'' c cont = liftIO $ connectionGetChunk' c cont

instance Universe (P.ListT IO) where
  connectionPut' c d = liftIO $ connectionPut c d
  connectionGetChunk'' c cont = liftIO $ connectionGetChunk' c cont

defaultImapSettings :: IMAPSettings
defaultImapSettings = IMAPSettings 30 10


