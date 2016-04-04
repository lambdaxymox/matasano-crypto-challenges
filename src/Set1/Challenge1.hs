{-# LANGUAGE FlexibleInstances #-}

import           Data.ByteString        as BS                    hiding (map, reverse, foldl)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as BSC8 (pack)
import           Data.Char (chr)
import           Data.Word
import           Data.Maybe
import           Data.Bits ((.|.))
import           Text.Megaparsec (many, parseMaybe, hexDigitChar)


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

data HexDigits = HexDigits { hexDigits :: [HexDigit] }
    deriving (Eq)

fromHexDigit :: HexDigit -> HexDigits
fromHexDigit = HexDigits . return

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

instance Show HexDigits where
    show hex =  foldl (\acc d -> acc ++ show d) "0x" $ hexDigits hex


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
            fromHexRep' acc (lower:upper:digits) = fromHexRep' ( (lowerNibble lower .|. upperNibble upper) : acc) digits


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
                    Just hex -> go hex
                    Nothing  -> Nothing
    where
        maybeHex :: String -> Maybe String
        maybeHex = parseMaybe (many hexDigitChar)

        go :: String -> Maybe HexDigits
        go s = fmap HexDigits $ sequence $ map (toHexDigit) s


extractHexBytes :: String -> Maybe [Word8]
extractHexBytes s = case (extractHexDigits s) of 
                    Just hex -> fromHexRep hex
                    Nothing  -> Nothing


base64 :: ByteString -> ByteString
base64 = Base64.encode

-- | The secret string as a string of hexadecimal digits.
secret' :: String
secret' = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

-- | The actual string after the hexadecimal has been parsed and packed.
secret :: ByteString
secret = BS.pack $ fromJust $ extractHexBytes secret'

secretBase64 :: ByteString
secretBase64 = BSC8.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

-- | Challenge 1
challenge1 = base64 secret