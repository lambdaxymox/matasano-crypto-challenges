module Crypto.FrequencyAnalysis.English
    (
        frequencyTable,
        frequencyTableW8,
        variationDist,
        score,
        mostLikelyChar,
    )
    where

import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Char8                    as BSC8
import           Crypto.FrequencyAnalysis
import           Data.Map                                 as Map
import           Util.ByteManipulation (xorWithChar, c2w)
import           Data.Word
import           Data.List (maximumBy, minimumBy)
import qualified Data.List                                as L (map)


frequencyTable :: Map.Map Char Double
frequencyTable = Map.fromList [
        ('E', 12.702), ('T', 9.056), ('A', 8.167), ('O', 7.507), ('I', 6.966), ('N', 6.749),
        ('S', 6.327),  ('H', 6.094), ('R', 5.987), ('D', 4.253), ('L', 4.025), ('C', 2.782),
        ('U', 2.758),  ('M', 2.406), ('W', 2.361), ('F', 2.228), ('G', 2.015), ('Y', 1.974),
        ('P', 1.929),  ('B', 1.492), ('V', 0.978), ('K', 0.772), ('J', 0.153), ('X', 0.150), 
        ('Q', 0.095),  ('Z', 0.074)
    ]

-- | The table below is taken from Pavel Mička's website, which cites Robert Lewand's Cryptological Mathematics.
--   See also: https://www.wikiwand.com/en/Letter_frequency
frequencyTableW8 :: Map.Map Word8 Double
frequencyTableW8 = Map.fromList [
        (c2w 'E', 12.702), (c2w 'T', 9.056), (c2w 'A', 8.167), (c2w 'O', 7.507), (c2w 'I', 6.966), (c2w 'N', 6.749),
        (c2w 'S', 6.327),  (c2w 'H', 6.094), (c2w 'R', 5.987), (c2w 'D', 4.253), (c2w 'L', 4.025), (c2w 'C', 2.782),
        (c2w 'U', 2.758),  (c2w 'M', 2.406), (c2w 'W', 2.361), (c2w 'F', 2.228), (c2w 'G', 2.015), (c2w 'Y', 1.974),
        (c2w 'P', 1.929),  (c2w 'B', 1.492), (c2w 'V', 0.978), (c2w 'K', 0.772), (c2w 'J', 0.153), (c2w 'X', 0.150), 
        (c2w 'Q', 0.095),  (c2w 'Z', 0.074)
    ]

englishLetters :: [Word8]
englishLetters = BS.unpack $ BSC8.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- | The 'variationDist' function calculates the variation distance between two probability distributions. The probability
--   distribution with the smallest variation distance from the reference distribution frequencyTableW8 is the most likely
--   one to be an English sentence.
variationDist :: Map.Map Word8 Double -> Double
variationDist qs =  Map.foldrWithKey variationDist' 0 qs
    where
        variationDist' key val acc = acc + term key val

        term key val = case Map.lookup key frequencyTableW8 of 
            -- If value is not present, we don't count it.
            Nothing -> 0.0 
            Just q  -> abs (val - q)


-- | The 'score' function calculates the variance of a string with respect to the English language
--   frequency table.
score :: BS.ByteString -> Double
score = scoreWith variationDist


-- | Assuming a ciphertext is the result of exclusive-oring a plaintext with a single character key, 
--   the 'mostLikelyChar' function guesses the most likely used character.
mostLikelyChar :: BS.ByteString -> ((Word8, BS.ByteString), Double)
mostLikelyChar = minCharWith score englishLetters
