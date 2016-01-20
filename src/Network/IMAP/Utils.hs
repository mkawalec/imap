module Network.IMAP.Utils where

import qualified Data.ByteString.Char8 as BSC
import System.Random

genRequestId :: IO BSC.ByteString
genRequestId = do
  randomGen <- newStdGen
  return $ BSC.pack . Prelude.take 9 $ randomRs ('a', 'z') randomGen
