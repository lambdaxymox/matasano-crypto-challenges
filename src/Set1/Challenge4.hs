module Set1.Challenge4
    (
        secret,
        challenge4,
        plainText,
        cipherText,
    )
    where

import           Util                                      (extractHexBytes)
import           Util.ByteManipulation                     (xorWithChar, w2c, c2w)
import           Util.IO                                   (readLines)
import           Crypto.FrequencyAnalysis.English
import           Data.ByteString                           as BS
import           Data.ByteString.Char8                     as BSC8
import qualified Data.List                                 as L (map, minimumBy, maximumBy)
import           Data.Traversable
import           Data.Word8


secret :: IO [BS.ByteString]
secret = L.map BS.pack <$> L.map extractHexBytes <$> L.map BSC8.unpack <$> readLines "Set1/ex4.txt"

scores :: [BS.ByteString] -> [((Word8, BS.ByteString), Double)]
scores = L.map mostLikelyChar

obtain :: [BS.ByteString] -> (Word8, BS.ByteString)
obtain strings = fst $ L.maximumBy (compare `on` snd) $ scores strings


cipherText :: IO (Word8, BS.ByteString)
cipherText = obtain <$> secret

plainText :: IO (Char, BS.ByteString)
plainText = do
    (ch, st)       <- cipherText
    let hexSt      = xorWithChar ch st
    let unpackedSt = BSC8.unpack hexSt
    return (w2c ch, BSC8.pack unpackedSt)


-- | Challenge 4
challenge4 :: IO (Char, BS.ByteString)
challenge4 = plainText
