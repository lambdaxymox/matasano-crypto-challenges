module Test.Crypto.FrequencyAnalysis.English where

import Test.HUnit

import Crypto.FrequencyAnalysis.English
import Util.ByteManipulation
import Data.ByteString.Char8 as B8

-- | Property Tests

-- | Unit tests
testsMostLikelyChar :: Test
testsMostLikelyChar = TestList [test1]

test1 :: Test
test1 = TestCase $ do     
                    let plainText  = B8.pack "I am regularly asked what the average Internet user can do to ensure his security. My first answer is usually 'Nothing; you're screwed'."
                    let ch         = c2w 'X'
                    let cipherText = xorWithChar ch plainText
                    let (guessedChar, guessedPlainText) = mostLikelyPair cipherText
                    let failStr = "Guessed wrong character:\n" 
                                        ++ "Guess: "  ++ show guessedChar ++ "\n"
                                        ++ "Answer: " ++ show ch 
                    assertEqual failStr ch guessedChar
                    assertEqual failStr plainText guessedPlainText
               

tests :: Test
tests = TestList [testsMostLikelyChar]

