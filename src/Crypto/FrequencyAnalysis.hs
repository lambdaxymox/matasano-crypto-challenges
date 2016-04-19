{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}

module Crypto.FrequencyAnalysis
    (
        computeWordCounts,
        relativeFreqs,
        scoreWith, 
        variationDist,
        rawBytes,
        transposeHistograms,
        charMap,
    )
    where

import           Util.ByteManipulation
import qualified Data.ByteString                  as BS
import qualified Data.Map.Strict                  as Map
import           Data.Word
import           Data.List                        (maximumBy, minimumBy)
import qualified Data.List                        as L (map, sortOn, zipWith)
import           Data.Char                        (toUpper)
import           Data.Function                    (on)



computeWordCounts :: Integral a => BS.ByteString -> Map.Map Word8 a
computeWordCounts = BS.foldl update Map.empty
    where
        update table key = case Map.lookup (upper key) table of
            Nothing  -> Map.insert (upper key) 1 table
            Just _   -> Map.adjust (+1) (upper key) table

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


rawBytes :: [Word8]
rawBytes = [0x00..0xFF]

-- | The 'variationDist' function calculates the variation distance between two probability distributions. The probability
--   distribution Q with the smallest variation distance from P is in some sense the closest one to P.
variationDist :: (Floating a, Ord a) => Map.Map Word8 a -> Map.Map Word8 a -> a
variationDist ps =  Map.foldrWithKey variationDist' 0
    where
        variationDist' key p acc = acc + term key p

        term key p = case Map.lookup key ps of 
            -- If value is not present, we don't count it.
            Nothing -> 0.0 
            Just q  -> abs (p - q)


transposeHistograms :: (Floating a, Ord a) => Map.Map Word8 a -> Map.Map Word8 a -> Map.Map Word8 a
transposeHistograms reference freqs = Map.fromList $ L.zipWith zipper newFreqs referenceTable
    where
        newFreqs           = L.sortOn snd $ Map.toList freqs
        referenceTable     = L.sortOn snd $ Map.toList reference
        zipper pair1 pair2 = (fst pair1, snd pair2)


charMap :: (Floating a, Ord a) => Map.Map Word8 a -> Map.Map Word8 a -> Map.Map Word8 Word8
charMap reference freqs = Map.fromList $ L.zipWith zipper newFreqs referenceTable 
    where
        newFreqs           = L.sortOn snd $ Map.toList freqs
        referenceTable     = L.sortOn snd $ Map.toList reference
        zipper pair1 pair2 = (fst pair1, fst pair2)
