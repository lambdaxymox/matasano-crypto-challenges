module Set1.Challenge4
    (
        secret,
        challenge4,
        plainText,
        cipherText,
    )
    where

import Util (extractHexBytes)
import Util.ByteManipulation (xorWithChar, readLines)
import Crypto.FrequencyAnalysis
import Data.ByteString                           as BS
import Data.ByteString.Char8                     as BSC8
import Data.List


secret :: IO [BS.ByteString]
secret = readLines "Set1/ex4.txt"

scores :: [BS.ByteString] -> [((Char, BS.ByteString), Double)]
scores strings = Data.List.map (mostLikelyChar) strings

obtain :: [BS.ByteString] -> (Char, BS.ByteString)
obtain strings = fst $ maximumBy (\p1 p2 -> compare (snd p1) (snd p2)) $ scores strings


cipherText :: IO (Char, BS.ByteString)
cipherText = obtain <$> secret

plainText :: IO (Char, BS.ByteString)
plainText = do
    (ch, st)       <- cipherText
    let hexSt      = xorWithChar ch st
    let unpackedSt = BSC8.unpack hexSt
    return (ch, BS.pack $ extractHexBytes unpackedSt)


challenge4 :: IO (Char, BS.ByteString)
challenge4 = plainText
