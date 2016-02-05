module Network.IMAP.Utils where

import Network.IMAP.Types
import qualified Data.ByteString.Char8 as BSC
import System.Random
import qualified Data.Text as T

import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar

import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.STM.Delay

genRequestId :: IO BSC.ByteString
genRequestId = do
  randomGen <- newStdGen
  return $ BSC.pack . Prelude.take 9 $ randomRs ('a', 'z') randomGen

readResults :: (MonadPlus m, MonadIO m, Universe m) =>
               TQueue CommandResult ->
               m CommandResult
readResults resultsQueue = do
  delay <- liftIO $ newDelay 1000
  let d_wait = do
        didComplete <- tryWaitDelay delay
        if didComplete
          then return . Tagged $ TaggedResult {
            commandId="noid",
            resultState=BAD,
            resultRest="Connection timeout"
          }
          else retry
      readResult = readTQueue $ resultsQueue

  nextResult <- liftIO . atomically $ d_wait `orElse` readResult
  case nextResult of
    Tagged _ -> return nextResult
    Untagged _ -> (return nextResult) `mplus` readResults resultsQueue

escapeText :: T.Text -> T.Text
escapeText t = T.replace "{" "\\{" $
             T.replace "}" "\\}" $
             T.replace "\"" "\\\"" $
             T.replace "\\" "\\\\" t


ifNotDisconnected :: (MonadPlus m, MonadIO m, Universe m) =>
                     IMAPConnection -> m CommandResult -> m CommandResult
ifNotDisconnected conn action = do
  connState <- liftIO . atomically . readTVar $ connectionState conn
  if isDisconnected connState
    then return . Tagged $ TaggedResult {
        commandId = "noid",
        resultState = BAD,
        resultRest = "Cannot post a command when watcher is disconnected"
      }
    else action
