module Main where

import           Test.HUnit
import qualified Test.Crypto.FrequencyAnalysis.English
import qualified Test.Util.ByteManipulation
import qualified System.Exit as Exit
import           Control.Monad                           (when)

tests :: Test
tests = TestList [
        TestLabel "Crypto.FrequencyAnalysis.English Tests" Test.Crypto.FrequencyAnalysis.English.tests,
        TestLabel "Test.Util.ByteManipulation Tests"       Test.Util.ByteManipulation.tests
    ]

main :: IO ()
main = do count <- runTestTT tests 
          when (failures count > 0) Exit.exitFailure