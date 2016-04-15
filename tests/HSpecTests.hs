module Main where

import Test.HUnit

import Test.Crypto.FrequencyAnalysis.English
import Test.Util.ByteManipulation



main :: IO Counts
main = do _ <- runTestTT Test.Crypto.FrequencyAnalysis.English.tests
          runTestTT      Test.Util.ByteManipulation.tests