module Test.Util.ByteManipulation where


import Test.HUnit

import Util.ByteManipulation
import Data.ByteString.Char8 as BSC8

-- | Property Tests

-- | Unit tests
test1 :: Test
test1 = TestCase $ do
                    assertEqual "" (hammingDistance 0x44 0x98) 5
                    assertEqual "" (hammingDistance 0xFF 0x00) 8 
                    assertEqual "" (hammingDistance 0xF0 0x0F) 8
                    assertEqual "" (hammingDistance 0xFF 0xFF) 0


test2 :: Test
test2 = TestCase $ do
                    let st1 = BSC8.pack "this is a test"
                    let st2 = BSC8.pack"wokka wokka!!!"
                    assertEqual "" (hammingDistanceBS st1 st2) 37


tests = TestList [test1, test2]