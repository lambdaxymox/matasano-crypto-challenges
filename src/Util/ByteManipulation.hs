{-# LANGUAGE FlexibleInstances #-}

{- |
Module: Util.ByteManipulation
Description: Utility functions for manipulating strings and hexadecimal digits.

This module contains utility functions for manipulating ByteStrings and for building 
cryptographic ciphers and cryptanalysis functions. It is primarily low-level tooling 
for manipulating strings and bitwise arithmetic operations on ByteStrings.
-}
module Util.ByteManipulation
    (
        c2w,
        w2c,
        base64,
        xor,
        maybeXor,
        repChar,
        repWord8,
        bslines,
        xorWithChar,
        xorWithKey,
        repStr,
        repByteStr,
        hammingDistance,
        hammingDistanceBS,
        maybeHammingDist,
        maybeHammingFracDist,
        meanHammingFracDist,
        maybeMeanHammingFracDist,
        padBS,
        padding,
        transposeAll
    )
    where

import           Prelude                 hiding (or, and)
import           Util.Hexadecimal
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as Base64
import qualified Data.ByteString.Char8   as BSC8
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy    as BSL
import           Data.Word
import           Data.Maybe
import           Data.Monoid
import qualified Data.Bits                as Bits (Bits(..)) 
import           Data.Bits                (Bits, (.|.), (.&.))
import           Text.Megaparsec          (many, parseMaybe, hexDigitChar)
import qualified Data.List                as L (map)
import           Text.Printf
import qualified Data.ByteString.Internal as BS (c2w, w2c)



-- Utility functions for manipulating strings and hexadecimal digits.
repChar :: Char -> Int -> BS.ByteString
repChar ch l = BSC8.replicate l ch

repWord8 :: Word8 -> Int -> BS.ByteString
repWord8 word l = BS.replicate l word


repStrBuilder :: Int -> BS.Builder -> BS.Builder
repStrBuilder 0 builder = mempty
repStrBuilder n builder = builder <> repStrBuilder (n-1) builder

repByteStr :: Int -> BS.ByteString -> BS.ByteString
repByteStr n bs = BSL.toStrict $ BS.toLazyByteString $ repStrBuilder n (BS.byteString bs)

repStr :: Int -> String -> BS.ByteString
repStr n st = BSL.toStrict $ BS.toLazyByteString $ repStrBuilder n (BS.string8 st)


-- | The 'base64' function encodes a ByteString into base64.
base64 :: BS.ByteString -> BS.ByteString
base64 = Base64.encode


bitwiseOp :: (Word8 -> Word8 -> Word8) -> BS.ByteString -> BS.ByteString -> BS.ByteString
bitwiseOp op bs1 bs2 = BS.pack $ BS.zipWith op bs1 bs2

-- | The 'xor' function takes two ByteStrings and computes their bitwise exclusive-or.
--   This is the unsafe version since it does not check whether the ByteStrings are of equal
--   length.
xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor = bitwiseOp (Bits.xor)

-- | Other bitwise arithmetic functions.

-- | The 'and' function takes two ByteStrings and computes their bitwise and.
--   This is the unsafe version since it does not check whether the ByteStrings are of equal
--   length.
or :: BS.ByteString -> BS.ByteString -> BS.ByteString
or = bitwiseOp (Bits..|.)

-- | The 'or' function takes two ByteStrings and computes their bitwise or.
--   This is the unsafe version since it does not check whether the ByteStrings are of equal
--   length.
and :: BS.ByteString -> BS.ByteString -> BS.ByteString
and = bitwiseOp (Bits..&.)


maybeOp :: (BS.ByteString -> BS.ByteString -> BS.ByteString) 
            -> BS.ByteString 
            -> BS.ByteString 
            -> Maybe BS.ByteString
maybeOp op bs1 bs2
    | BS.length bs1 == BS.length bs2 = Just $ op bs1 bs2
    | otherwise                      = Nothing

-- | 'maybeXor' bitwise exclusive-or's together two ByteStrings of equal length. Otherwise it returns nothing.
maybeXor :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
maybeXor = maybeOp xor

-- | 'maybeAnd' bitwise and's together two ByteStrings of equal length. Otherwise it returns nothing.
maybeAnd :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
maybeAnd = maybeOp or

-- | 'maybeOr' bitwise or's together two ByteStrings of equal length. Otherwise it returns nothing.
maybeOr :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
maybeOr = maybeOp and



withChar :: Word8 -> BS.ByteString -> BS.ByteString
withChar ch st = repWord8 ch (BS.length st)


opWithChar :: (BS.ByteString -> BS.ByteString -> BS.ByteString)
            -> Word8
            -> BS.ByteString 
            -> BS.ByteString 
opWithChar op ch st = st `op` withChar ch st

-- | 'xorWithChar' exclusive-or's a string with a string of repeating characters
--   of length equal to the original string. 
xorWithChar :: Word8 -> BS.ByteString -> BS.ByteString
xorWithChar = opWithChar xor

-- | Other keyed arithmetic operation with repeating characters.
andWithChar :: Word8 -> BS.ByteString -> BS.ByteString
andWithChar = opWithChar and

orWithChar :: Word8 -> BS.ByteString -> BS.ByteString
orWithChar = opWithChar or


withKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
withKey key st = repKey
    where
        repCount  = BS.length st `div` BS.length key
        remainder = BS.length st `mod` BS.length key
        repKey    = repByteStr repCount key <> BS.take remainder key

opWithKey :: (BS.ByteString -> BS.ByteString -> BS.ByteString)
            -> BS.ByteString 
            -> BS.ByteString 
            -> BS.ByteString
opWithKey op st key = st `op` withKey key st

-- | The function 'xorWithKey' exclusive-or's a bytestring together with a key that is repeated
--   as many times as needed to match the length of the bytestring. If the string length is not an even multiple
--   of the key length, the remainder of the string is exlusive-ored with with a prefix of the key to pad the 
--   key string's length to match the encoded string length. 
xorWithKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorWithKey = opWithKey xor

-- | Other keyed arithmetic operations.
orWithKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
orWithKey = opWithKey or

andWithKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
andWithKey = opWithKey and


-- | Splits a ByteString delimited by newlines.
bslines :: BS.ByteString -> [BS.ByteString]
bslines = BS.split (c2w '\n')


padBS :: Int -> BS.ByteString -> BS.ByteString
padBS n st = st <> repWord8 0x00 (n - BS.length st)

padding :: Int -> BS.ByteString
padding n = repWord8 0x00 n


-- | The 'hammingDistance' function calculates the Hamming distance between two numbers. The Hamming distance
--   is the number of bits where two numbers differ.
hammingDistance :: Integral b => Word8 -> Word8 -> b
hammingDistance x y =  loop (x `Bits.xor` y) 0
    where
        loop 0   dist = dist
        loop val dist = 
            let
                val'  = val .&. (val - 1)
                dist' = dist + 1
            in 
                loop val' dist'


-- | The 'hammingDistance' function calculates the Hamming distance between strings. The Hamming distance
--   is the number of bits where two values differ.
hammingDistanceBS :: Integral b => BS.ByteString -> BS.ByteString -> b
hammingDistanceBS st1 st2 = sum $ BS.zipWith (\ch1 ch2 -> hammingDistance ch1 ch2) st1 st2


maybeHammingDist :: Integral b => BS.ByteString -> BS.ByteString -> Maybe b
maybeHammingDist bs1 bs2
    | BS.length bs1 == BS.length bs2 = Just $ hammingDistanceBS bs1 bs2
    | otherwise                      = Nothing


hammingFractionalDist :: (Floating b, Ord b) => BS.ByteString -> BS.ByteString -> b
hammingFractionalDist bs1 bs2 = fromIntegral distance / fromIntegral size
    where
        distance = hammingDistanceBS bs1 bs2
        size     = BS.length bs1


maybeHammingFracDist :: (Floating b, Ord b) => BS.ByteString -> BS.ByteString -> Maybe b
maybeHammingFracDist bs1 bs2
    | BS.length bs1 == BS.length bs2 = Just $ hammingFractionalDist bs1 bs2
    | otherwise                      = Nothing


meanHammingFracDist :: (Floating b, Ord b) => [BS.ByteString] -> b
meanHammingFracDist bss = fromIntegral dist / fromIntegral len
    where
        edit (bs1:[])      = 0
        edit []            = 0
        edit (bs1:bs2:bss) = hammingDistanceBS bs1 bs2 + edit (bs2:bss)

        dist = edit bss
        len  = sum $ map (BS.length) bss


maybeMeanHammingFracDist :: (Floating b, Ord b) => [BS.ByteString] -> Maybe b
maybeMeanHammingFracDist bss
    | null bss       = Nothing
    | sameLength bss = Just $ meanHammingFracDist bss
    | otherwise      = Nothing
    where
        bs1 = head bss
        sameLength bss = all (\bs -> BS.length bs == BS.length bs1) bss


transpose :: Int -> Int -> BS.ByteString -> BS.ByteString
transpose n j st = BS.pack $ L.map (\i -> BS.index st i) [i | i <- [0..BS.length st - 1], i `mod` n == j]

transposeAll :: Int -> BS.ByteString -> [BS.ByteString]
transposeAll n st = L.map (\j -> transpose n j st) [0..n-1]