module Main where

import           Test.HUnit
import qualified Test.Crypto.FrequencyAnalysis.English
import qualified Test.Util.ByteManipulation
import qualified System.Exit as Exit

tests :: Test
tests = TestList [
        TestLabel "Crypto.FrequencyAnalysis.English Tests" Test.Crypto.FrequencyAnalysis.English.tests,
        TestLabel "Test.Util.ByteManipulation Tests"       Test.Util.ByteManipulation.tests
    ]

main = do count <- runTestTT tests 
          if failures count > 0 then Exit.exitFailure else return ()