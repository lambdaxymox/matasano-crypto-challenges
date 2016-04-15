module Crypto.FrequencyAnalysis.English
    (
        frequencyTable,
        frequencyTableW8,
        score,
        mostLikelyChar,
        mostLikelyPair,
    )
    where

import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Char8                    as BSC8
import           Crypto.FrequencyAnalysis
import           Data.Map                                 as Map
import           Util.ByteManipulation                    (c2w)
import           Data.Word


frequencyTable :: Map.Map Char Double
frequencyTable = Map.fromList [
        ('E', 0.12702), ('T', 0.09056), ('A', 0.08167), ('O', 0.07507), ('I', 0.06966), ('N', 0.06749),
        ('S', 0.06327), ('H', 0.06094), ('R', 0.05987), ('D', 0.04253), ('L', 0.04025), ('C', 0.02782),
        ('U', 0.02758), ('M', 0.02406), ('W', 0.02361), ('F', 0.02228), ('G', 0.02015), ('Y', 0.01974),
        ('P', 0.01929), ('B', 0.01492), ('V', 0.00978), ('K', 0.00772), ('J', 0.00153), ('X', 0.00150), 
        ('Q', 0.00095), ('Z', 0.00074)
    ]

-- | The table below is taken from Pavel Mička's website, which cites Robert Lewand's Cryptological Mathematics.
--   See also: https://www.wikiwand.com/en/Letter_frequency
frequencyTableW8 :: Map.Map Word8 Double
frequencyTableW8 = Map.fromList [
        (c2w 'E', 0.12702), (c2w 'T', 0.09056), (c2w 'A', 0.08167), (c2w 'O', 0.07507), (c2w 'I', 0.06966), (c2w 'N', 0.06749),
        (c2w 'S', 0.06327), (c2w 'H', 0.06094), (c2w 'R', 0.05987), (c2w 'D', 0.04253), (c2w 'L', 0.04025), (c2w 'C', 0.02782),
        (c2w 'U', 0.02758), (c2w 'M', 0.02406), (c2w 'W', 0.02361), (c2w 'F', 0.02228), (c2w 'G', 0.02015), (c2w 'Y', 0.01974),
        (c2w 'P', 0.01929), (c2w 'B', 0.01492), (c2w 'V', 0.00978), (c2w 'K', 0.00772), (c2w 'J', 0.00153), (c2w 'X', 0.00150), 
        (c2w 'Q', 0.00095), (c2w 'Z', 0.00074)
    ]

englishLetters :: [Word8]
englishLetters = BS.unpack $ BSC8.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- | The 'scoreFunc' function simply adds up the relative frequency that each character in a string with respect
--   to the English langauge frequency table. 
scoreFunc :: Map.Map Word8 Double -> Double
scoreFunc = Map.foldrWithKey scoreFunc' 0
    where
        scoreFunc' k _ acc = acc + term k

        term k = case Map.lookup k frequencyTableW8 of
            Nothing -> 0.0
            Just q  -> q


-- | The 'score' function calculates the variance of a string with respect to the English language
--   frequency table.
score :: BS.ByteString -> Double
score = scoreWith scoreFunc


-- | Assuming a ciphertext is the result of exclusive-oring a plaintext with a single character key, 
--   the 'mostLikelyChar' function guesses the most likely used character.
mostLikelyChar :: BS.ByteString -> ((Word8, BS.ByteString), Double)
mostLikelyChar = maxCharWith score englishLetters

mostLikelyPair :: BS.ByteString -> (Word8, BS.ByteString)
mostLikelyPair = fst . mostLikelyChar