module Main where

import System.Exit (exitFailure)
import qualified Network.IMAP.Tests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [Network.IMAP.Tests.tests]
