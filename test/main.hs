module Main where

import Test.Utils (getConn)
import qualified Network.IMAP.Tests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  conn <- getConn
  defaultMain $ testGroup "Tests"
    [(Network.IMAP.Tests.tests conn)]
