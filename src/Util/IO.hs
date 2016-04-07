module Util.IO
    (
        readLines,
        readBS,
        sizedBlocks,
        exactlyKSizedBlocks,
        getKPaddedBlocks, 
    )
    where

import qualified Data.ByteString       as BS
import           Util.ByteManipulation (bslines, padding)
import           System.IO
import           Data.Monoid


readLines :: String -> IO [BS.ByteString]
readLines fname = do
    file     <- openFile fname ReadMode
    contents <- BS.hGetContents file
    strings  <- return $ bslines contents
    hClose file 
    return strings


readBS :: String -> IO BS.ByteString
readBS fname = do
    file <- openFile fname ReadMode
    contents <- BS.hGetContents file
    hClose file
    return contents


unsafeSizedBlock :: Int -> BS.ByteString -> BS.ByteString
unsafeSizedBlock size st = BS.take size st


unsafeSizedBlocks :: Int -> BS.ByteString -> [BS.ByteString]
unsafeSizedBlocks size st
    | BS.null st   = []
    | otherwise = BS.take size st : unsafeSizedBlocks size (BS.drop size st)


sizedBlocks :: Int -> BS.ByteString -> Either String [BS.ByteString]
sizedBlocks size st
    | size < 0  = Left "Chunk size must be positive."
    | otherwise = Right $ unsafeSizedBlocks size st


unsafeGetKBlocks :: Int -> Int -> BS.ByteString -> [BS.ByteString]
unsafeGetKBlocks k size st
    | BS.null st   = []
    | k == 0    = []
    | otherwise = unsafeSizedBlock size st : unsafeGetKBlocks (k-1) size (BS.drop size st)


atMostKSizedBlocks :: Int -> Int -> BS.ByteString -> Either String [BS.ByteString]
atMostKSizedBlocks k size st
    | k < 0     = Left "Number of chunks must be positive."
    | size < 0  = Left "Chunk size must be positive."
    | otherwise = Right $ unsafeGetKBlocks k size st


exactlyKSizedBlocks :: Int -> Int -> BS.ByteString -> Either String [BS.ByteString]
exactlyKSizedBlocks k size st
    | chunkCount < k = Left "Not enough chunks in string."
    | otherwise      = atMostKSizedBlocks k size st
    where
        chunkCount = (BS.length st) `div` size


--unsafePaddedKBlocks :: Int -> Int -> BS.ByteString -> [BS.ByteString]
--unsafePaddedKBlocks k size st


getKPaddedBlocks :: Int -> Int -> BS.ByteString -> Either String [BS.ByteString]
getKPaddedBlocks k size st 
    | chunkCount < k = exactlyKSizedBlocks k size paddedStr
    | otherwise      = exactlyKSizedBlocks k size st
    where
        chunkCount       = (BS.length st) `div` size
        remainingBytes   = ((BS.length st) `mod` size) -- Pad the last chunk of the string to an even multiple of chunk size
        padBytes         = size - remainingBytes       -- The number of bytes to to pad the last chunk of the original string to size.
        neededChunks     = k - chunkCount - 1          -- The number of chunks worth of padding after the remainder is padded. 
        neededChunkBytes = size * neededChunks         -- The byte size of the needed chunks. 
        neededBytes      = neededChunkBytes + padBytes -- The total number of padding bytes needed.
        paddedStr        = st <> padding neededBytes 
