{-# LANGUAGE FlexibleInstances #-}

{- |
Module: Util.ByteManipulation
Description: Utility functions for manipulating strings and hexadecimal digits.

This module contains utility functions for manipulating ByteStrings and for building 
cryptographic ciphers and cryptanalysis functions. It is primarily low-level tooling 
for extracting hexadecimal digits from strings and bitwise arithmetic operation on ByteStrings.
-}
module Util.ByteManipulation
    (
        base64,
        xor,
        maybeXor,
        repChar,
        repWord8,
        bslines,
        readLines,
        xorWithChar,
        xorWithKey,
        repStr,
        repByteStr,
    )
    where

import           Util.Hexadecimal
import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Base64                           as Base64
import qualified Data.ByteString.Char8                            as BSC8
import qualified Data.ByteString.Builder                          as BS
import qualified Data.ByteString.Lazy                             as BSL
import           Data.Word
import           Data.Maybe
import           Data.Monoid
import qualified Data.Bits                                        as Bits (xor) 
import           Data.Bits ((.|.))
import           Text.Megaparsec (many, parseMaybe, hexDigitChar)
import           Text.Printf
import qualified Data.ByteString.Internal                         as BS (c2w, w2c)
import           System.IO


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


-- | The 'xor' function takes two ByteStrings and computes their bitwise exclusive-or.
--   This is the unsafe version since it does not check whether the ByteStrings are of equal
--   length.
xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor bs1 bs2 = BS.pack $ BS.zipWith (Bits.xor) bs1 bs2


-- | 'maybeXor' exclusive-or's together two ByteStrings of equal length. Otherwise it returns nothing.
maybeXor :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
maybeXor bs1 bs2 
    | BS.length bs1 == BS.length bs2 = Just $ xor bs1 bs2
    | otherwise                      = Nothing


-- | The 'singleCharXor'
xorWithChar :: Char -> BS.ByteString -> BS.ByteString
xorWithChar ch st = st `xor` repChar ch (BS.length st) 


xorWithKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorWithKey st key = repKey `xor` st
    where
        repCount  = BS.length st `div` BS.length key
        remainder = BS.length st `mod` BS.length key
        repKey    = repByteStr repCount key <> BS.take remainder key


bslines :: BS.ByteString -> [BS.ByteString]
bslines = BS.split (c2w '\n')


readLines :: String -> IO [BS.ByteString]
readLines fname = do
    file     <- openFile fname ReadMode
    contents <- BS.hGetContents file
    strings  <- return $ bslines contents
    hClose file 
    return strings
