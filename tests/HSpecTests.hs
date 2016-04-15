module Main where

import Test.HUnit

import Test.Crypto.FrequencyAnalysis.English

main :: IO Counts
main = runTestTT tests