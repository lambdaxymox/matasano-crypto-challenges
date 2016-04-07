module Crypto.FrequencyAnalysis
    (
        computeWordCounts,
        relativeFreqs,
        scoreWith, 
        --mostLikelyChar,
    )
    where

import           Util.ByteManipulation
import           Util.Hexadecimal
import           Data.Maybe
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC8
import qualified Data.Map.Strict                  as Map
import           Data.Word
import           Data.List (maximumBy, minimumBy)
import           Data.Char (chr, toUpper)



computeWordCounts :: Integral a => BS.ByteString -> Map.Map Word8 a
computeWordCounts bs = BS.foldl update Map.empty bs
    where
        update table key = case Map.lookup (upper key) table of
            Nothing  -> Map.insert (upper key) 1 table
            Just val -> Map.adjust (\val -> val+1) (upper key) table

        upper = c2w . toUpper . w2c


toFreqs :: (Integral a, Floating b, Ord b) => Map.Map Word8 a -> Map.Map Word8 b
toFreqs wmap = Map.map freq wmap
    where
        total = Map.foldl (\acc val -> acc + val) 0 wmap

        freq val = fromIntegral val / fromIntegral total


relativeFreqs :: (Floating a, Ord a) => BS.ByteString -> Map.Map Word8 a
relativeFreqs = toFreqs . computeWordCounts


scoreWith :: (Floating a, Ord a) => (Map.Map Word8 a -> a) -> BS.ByteString -> a
scoreWith scoreFunc = scoreFunc . relativeFreqs


--mostLikelyChar = id

