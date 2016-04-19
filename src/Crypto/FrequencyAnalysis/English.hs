module Crypto.FrequencyAnalysis.English
    (
        frequencyTable,
        mostLikelyChar,
        mostLikelyWord8,
        score,
    )
    where

import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Char8                    as BSC8
import           Crypto.FrequencyAnalysis
import qualified Crypto.FrequencyAnalysis.BreakXorCipher  as Xor
import           Data.Map                                 as Map
import           Util.ByteManipulation                    (c2w)
import           Data.Word
import qualified Data.List                                as L (sortOn)


-- | The table below is taken from Pavel Mička's website, which cites Robert Lewand's Cryptological Mathematics.
--   See also: https://www.wikiwand.com/en/Letter_frequency
frequencyTable :: Map.Map Word8 Double
frequencyTable = Map.fromList [
        (c2w 'E', 0.12702), (c2w 'T', 0.09056), (c2w 'A', 0.08167), (c2w 'O', 0.07507), (c2w 'I', 0.06966), (c2w 'N', 0.06749),
        (c2w 'S', 0.06327), (c2w 'H', 0.06094), (c2w 'R', 0.05987), (c2w 'D', 0.04253), (c2w 'L', 0.04025), (c2w 'C', 0.02782),
        (c2w 'U', 0.02758), (c2w 'M', 0.02406), (c2w 'W', 0.02361), (c2w 'F', 0.02228), (c2w 'G', 0.02015), (c2w 'Y', 0.01974),
        (c2w 'P', 0.01929), (c2w 'B', 0.01492), (c2w 'V', 0.00978), (c2w 'K', 0.00772), (c2w 'J', 0.00153), (c2w 'X', 0.00150), 
        (c2w 'Q', 0.00095), (c2w 'Z', 0.00074)
    ]

--sortedEnglishTable :: [(Word8, Double)]
--sortedEnglishTable = L.sortOn snd $ toList frequencyTable

englishLetters :: [Word8]
englishLetters = BS.unpack $ BSC8.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- | The 'cipherScoreFunc' functions looks for the most English-looking cipher-text using frequency analysis. It guesses
--   that the ciphertext was encoded using a substitution cipher so the bytes that occur in the cipher-text can be compareds to
--   the bytes that occur in English by relative frequency.  
cipherScoreFunc :: Map.Map Word8 Double -> Double
cipherScoreFunc freqs = Map.foldrWithKey scoreFunc' 0.0 newFreqs 
    where
        newFreqs = transposeHistograms frequencyTable freqs

        scoreFunc' k _ acc = acc + term k

        term k = case Map.lookup k newFreqs of
            Nothing -> 0.0
            Just q  -> q


-- | The 'cipherScore' function 
cipherScore :: BS.ByteString -> Double
cipherScore = scoreWith cipherScoreFunc


score :: BS.ByteString -> Double
score = Xor.score frequencyTable

-- | Assuming a ciphertext is the result of exclusive-oring a plaintext with a single character key, 
--   the 'mostLikelyChar' function guesses the most likely used character.
mostLikelyChar :: BS.ByteString -> ((Word8, BS.ByteString), Double)
mostLikelyChar st = Xor.breakXorCharKeyWith frequencyTable englishLetters st

mostLikelyWord8 :: BS.ByteString -> ((Word8, BS.ByteString), Double)
mostLikelyWord8 st = Xor.breakXorCharKeyWith frequencyTable rawBytes st