module Set1.Challenge4
    (
        secret,
        challenge4,
        plainText,
        cipherText,
    )
    where

import Util                                      (extractHexBytes)
import Util.ByteManipulation                     (xorWithChar, w2c)
import Util.IO                                   (readLines)
import Crypto.FrequencyAnalysis.English
import Data.ByteString                           as BS
import Data.ByteString.Char8                     as BSC8
import Data.List
import Data.Word8


secret :: IO [BS.ByteString]
secret = readLines "Set1/ex4.txt"

scores :: [BS.ByteString] -> [((Word8, BS.ByteString), Double)]
scores strings = Data.List.map (mostLikelyChar) strings

obtain :: [BS.ByteString] -> (Word8, BS.ByteString)
obtain strings = fst $ maximumBy (\p1 p2 -> compare (snd p1) (snd p2)) $ scores strings


cipherText :: IO (Word8, BS.ByteString)
cipherText = obtain <$> secret

plainText :: IO (Char, BS.ByteString)
plainText = do
    (ch, st)       <- cipherText
    let hexSt      = xorWithChar ch st
    let unpackedSt = BSC8.unpack hexSt
    return (w2c ch, BS.pack $ extractHexBytes unpackedSt)


challenge4 :: IO (Char, BS.ByteString)
challenge4 = plainText
