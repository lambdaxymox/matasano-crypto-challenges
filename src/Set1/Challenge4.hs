module Set1.Challenge4
    (
        secret,
        challenge4,
        cipherText,
        answer,
    )
    where

import           Util                                      (extractHexBytes)
import           Util.ByteManipulation                     (xorWithChar, w2c, c2w)
import           Util.IO                                   (readLines)
import           Crypto.FrequencyAnalysis.English
import           Crypto.FrequencyAnalysis
import           Data.ByteString                           as BS
import           Data.ByteString.Char8                     as BSC8
import qualified Data.List                                 as L (map, minimumBy, maximumBy)
import           Data.Traversable
import           Data.Word8
import           Data.Function                             (on)
import           System.IO.Unsafe


secret :: IO [BS.ByteString]
secret = L.map (BS.pack . extractHexBytes . BSC8.unpack) <$> readLines "Set1/ex4.txt"

scores :: [BS.ByteString] -> [((Word8, BS.ByteString), Double)]
scores = L.map mostLikelyWord8

obtain :: [BS.ByteString] -> (Word8, BS.ByteString)
obtain strings = fst $ L.maximumBy (compare `on` snd) $ scores strings

cipherText :: IO (Word8, BS.ByteString)
cipherText = obtain <$> secret

answer :: (Word8, BS.ByteString)
answer = (53, BSC8.pack "Now that the party is jumping\n")

-- | Challenge 4
challenge4 :: IO (Word8, BS.ByteString)
challenge4 = do
    (ch, st)  <- cipherText
    let plainText = xorWithChar ch st
    return (ch, plainText)
