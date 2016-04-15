{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC  -fno-warn-unused-binds #-}

{- |
Module: Util.Hexadecimal
Description: This module contains functions for parsing and representing raw numbers
             and strings as hexadecimal characters.
-}
module Util.Hexadecimal
    (
        ToHexadecimal(..),
        maybeExtractHexBytes,
        extractHexBytes,
        c2w,
        w2c,
        hex,
        hexBS,
    )
    where

import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Char8                            as BSC8
import           Data.Word
import           Data.Maybe
import           Data.Bits ((.|.))
import           Text.Megaparsec (many, parseMaybe, hexDigitChar)
import           Text.Printf
import qualified Data.ByteString.Internal                         as BS (c2w, w2c)

data HexDigit = Hex0 | Hex1 | Hex2 | Hex3 | Hex4 | Hex5 | Hex6 | Hex7 
              | Hex8 | Hex9 | HexA | HexB | HexC | HexD | HexE | HexF
    deriving (Eq)


instance Show HexDigit where
    show digit = case digit of
        Hex0 -> "0"
        Hex1 -> "1"
        Hex2 -> "2"
        Hex3 -> "3"
        Hex4 -> "4"
        Hex5 -> "5"
        Hex6 -> "6"
        Hex7 -> "7"
        Hex8 -> "8"
        Hex9 -> "9"
        HexA -> "A"
        HexB -> "B"
        HexC -> "C"
        HexD -> "D"
        HexE -> "E"
        HexF -> "F"


class ToHexDigit a where
    toHexDigit :: a -> Maybe HexDigit


instance ToHexDigit Char where
    toHexDigit ch = 
        case ch of
            '0' -> Just Hex0
            '1' -> Just Hex1
            '2' -> Just Hex2
            '3' -> Just Hex3
            '4' -> Just Hex4
            '5' -> Just Hex5
            '6' -> Just Hex6
            '7' -> Just Hex7
            '8' -> Just Hex8
            '9' -> Just Hex9
            'A' -> Just HexA
            'B' -> Just HexB
            'C' -> Just HexC
            'D' -> Just HexD
            'E' -> Just HexE
            'F' -> Just HexF
            'a' -> Just HexA
            'b' -> Just HexB
            'c' -> Just HexC
            'd' -> Just HexD
            'e' -> Just HexE
            'f' -> Just HexF
            _   -> Nothing

instance ToHexDigit String where
    toHexDigit ch = 
        case ch of
            "0" -> Just Hex0
            "1" -> Just Hex1
            "2" -> Just Hex2
            "3" -> Just Hex3
            "4" -> Just Hex4
            "5" -> Just Hex5
            "6" -> Just Hex6
            "7" -> Just Hex7
            "8" -> Just Hex8
            "9" -> Just Hex9
            "A" -> Just HexA
            "B" -> Just HexB
            "C" -> Just HexC
            "D" -> Just HexD
            "E" -> Just HexE
            "F" -> Just HexF
            "a" -> Just HexA
            "b" -> Just HexB
            "c" -> Just HexC
            "d" -> Just HexD
            "e" -> Just HexE
            "f" -> Just HexF
            _   -> Nothing

data HexDigits = HexDigits { hexDigits :: [HexDigit] }
    deriving (Eq)


instance Show HexDigits where
    show hexVal =  foldl (\acc d -> acc ++ show d) "0x" $ hexDigits hexVal

fromHexDigit :: HexDigit -> HexDigits
fromHexDigit = HexDigits . return


class HexRep a where
    fromHexRep :: HexDigits -> Maybe a


instance HexRep Word8 where
    fromHexRep digits = fromHexRep' $ hexDigits digits
        where
            fromHexRep' (digit:[])       = Just (lowerNibble digit)
            fromHexRep' (upper:lower:[]) = Just (upperNibble upper .|. lowerNibble lower)
            fromHexRep' _                = Nothing

instance HexRep [Word8] where
    fromHexRep digits = Just $ fromHexRep' [] $ reverse $ hexDigits digits
        where
            fromHexRep' acc []                   = acc
            fromHexRep' acc (digit:[])           = (lowerNibble digit .|. upperNibble Hex0) : acc
            fromHexRep' acc (lower:upper:ds) = fromHexRep' ( (lowerNibble lower .|. upperNibble upper) : acc) ds


-- | The 'ToHexadecimal' class is for producing hexadecimal string representations of data.
class ToHexadecimal a where
    toHex :: a -> String

    toHexBS :: a -> BS.ByteString
    toHexBS = BSC8.pack . toHex

instance ToHexadecimal Word8 where
    toHex ch = printf "%.2x" ch :: String

instance ToHexadecimal Char where
    toHex ch = printf "%.2x" ch :: String

instance ToHexadecimal String where
    toHex st = concatMap toHex st :: String

instance ToHexadecimal [Word8] where
    toHex st = concatMap toHex st :: String

instance ToHexadecimal BS.ByteString where
    toHex bs = toHex $ BS.unpack bs


lowerNibble :: HexDigit -> Word8
lowerNibble d = case d of
    Hex0 -> 0x00 :: Word8
    Hex1 -> 0x01 :: Word8
    Hex2 -> 0x02 :: Word8
    Hex3 -> 0x03 :: Word8
    Hex4 -> 0x04 :: Word8
    Hex5 -> 0x05 :: Word8
    Hex6 -> 0x06 :: Word8
    Hex7 -> 0x07 :: Word8
    Hex8 -> 0x08 :: Word8
    Hex9 -> 0x09 :: Word8
    HexA -> 0x0A :: Word8
    HexB -> 0x0B :: Word8
    HexC -> 0x0C :: Word8
    HexD -> 0x0D :: Word8
    HexE -> 0x0E :: Word8
    HexF -> 0x0F :: Word8


upperNibble :: HexDigit -> Word8
upperNibble d = case d of
    Hex0 -> 0x00 :: Word8
    Hex1 -> 0x10 :: Word8
    Hex2 -> 0x20 :: Word8
    Hex3 -> 0x30 :: Word8
    Hex4 -> 0x40 :: Word8
    Hex5 -> 0x50 :: Word8
    Hex6 -> 0x60 :: Word8
    Hex7 -> 0x70 :: Word8
    Hex8 -> 0x80 :: Word8
    Hex9 -> 0x90 :: Word8
    HexA -> 0xA0 :: Word8
    HexB -> 0xB0 :: Word8
    HexC -> 0xC0 :: Word8
    HexD -> 0xD0 :: Word8
    HexE -> 0xE0 :: Word8
    HexF -> 0xF0 :: Word8


extractHexDigits :: String -> Maybe HexDigits
extractHexDigits s = case (maybeHex s) of
                    Just hexVal -> go hexVal
                    Nothing     -> Nothing
    where
        maybeHex :: String -> Maybe String
        maybeHex = parseMaybe (many hexDigitChar)

        go :: String -> Maybe HexDigits
        go st = fmap HexDigits $ sequence $ map (toHexDigit) st


-- | The 'maybeExtractHexBytes' function extracts raw hexadecimal digits from a string. 
maybeExtractHexBytes :: String -> Maybe [Word8]
maybeExtractHexBytes st = case (extractHexDigits st) of 
                    Just hexVal -> fromHexRep hexVal
                    Nothing  -> Nothing


-- | The 'extractHexBytes' function extracts raw hexadecimal digits from a string. 
extractHexBytes :: String -> [Word8]
extractHexBytes = fromJust . maybeExtractHexBytes


-- | Convert between Char and Word8 and back.
c2w :: Char -> Word8
c2w = BS.c2w

w2c :: Word8 -> Char
w2c = BS.w2c

hex :: String -> [Word8]
hex = map c2w

hexBS :: BS.ByteString -> [Word8]
hexBS = hex . BSC8.unpack
