module Set1.Challenge3
    (
        challenge3,
        candidates,
        secret,
        answer,
        score,
    )
    where

import           Util.ByteManipulation
import           Data.Maybe
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC8
import qualified Data.Map.Strict          as Map
import           Data.Word
import           Data.List (maximumBy, minimumBy)
import           Data.Char (chr, toUpper)


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


computeWordCounts :: BS.ByteString -> Map.Map Word8 Int
computeWordCounts bs = BS.foldl update Map.empty bs
    where
        update table key = case Map.lookup (upper key) table of
            Nothing  -> Map.insert (upper key) 1 table
            Just val -> Map.adjust (\val -> val+1) (upper key) table

        upper = c2w . toUpper . w2c


toFreqs :: Map.Map Word8 Int -> Map.Map Word8 Double
toFreqs wmap = Map.map freq wmap
    where
        total = Map.foldl (\acc val -> acc + val) 0 wmap

        freq val = fromIntegral val / fromIntegral total


computeFreqs :: BS.ByteString -> Map.Map Word8 Double
computeFreqs = toFreqs . computeWordCounts


-- | The 'computeScore' function calculates the variance of the frequencies with respect to the English language
--   frequency table.
computeScore :: Map.Map Word8 Double -> Double
computeScore table =  Map.foldrWithKey variance 0 table
    where
        variance key val acc = acc + term key val

        term key val = case Map.lookup key frequencyTableW8 of 
            -- If value is not present, we're further away from being an English sentence.
            Nothing   -> 0.0 
            Just mean -> (val - mean)^2


score :: BS.ByteString -> Double
score = computeScore . computeFreqs


secret :: BS.ByteString
secret = BS.pack $ fromJust $ extractHexBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

candidates :: Map.Map Char BS.ByteString
candidates = Map.fromList $ map (\ch -> (ch, secret `xor` repChar ch (BS.length secret))) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


answerS :: BS.ByteString
answerS = BSC8.pack "Cooking MC's like a pound of bacon"

answer :: (Char, BS.ByteString)
answer = ('X', answerS)

challenge3 :: (Char, BS.ByteString)
challenge3 = (bestChar, bestString)
    where
        scores     = Map.map score candidates
        bestChar   = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $ Map.toList scores 
        bestString = secret `xor` repChar bestChar (BS.length secret)

