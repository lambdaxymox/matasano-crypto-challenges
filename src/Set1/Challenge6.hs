module Set1.Challenge6
    (
        secret,
        blocks,
        mostProbableKeySize,
        chunks,
        cipherTextBlocks,
        challenge6,

    )
    where

import           Crypto.FrequencyAnalysis.English (mostLikelyChar)
import           Util.Util                        (right)
import           Util.IO                          (getKPaddedBlocks, readBS, sizedBlocks)
import           Util.ByteManipulation            (xorWithKey, maybeMeanHammingFracDist)
import qualified Data.ByteString                  as BS
import           Data.Maybe                       (fromJust)
import qualified Data.List                        as L (minimumBy, map)
import           Data.Monoid


secret :: IO BS.ByteString
secret = readBS "Set1/ex6.txt"


blocks :: Int -> Int -> BS.ByteString -> [BS.ByteString]
blocks k keySize bs = right $ getKPaddedBlocks k keySize bs


usingBlockCount :: Int -> [Int] -> BS.ByteString -> Int
usingBlockCount n keySizes bs = L.minimumBy (\ks1 ks2 -> compare (fracDist ks1) (fracDist ks2)) keySizes
    where
        fracDist keySize = fromJust $ maybeMeanHammingFracDist $ blocks n keySize bs


guessKeySize4 :: [Int] -> BS.ByteString -> Int
guessKeySize4 = usingBlockCount 4


-- | Challenge 6
keySizes :: [Int]
keySizes = [2..32]

mostProbableKeySize :: IO Int
mostProbableKeySize = guessKeySize4 keySizes <$> secret

chunks :: IO [BS.ByteString]
chunks = fmap (right) $ sizedBlocks <$> mostProbableKeySize <*> secret

transpose :: Int -> Int -> BS.ByteString -> BS.ByteString
transpose n j st = BS.pack $ L.map (\i -> BS.index st i) [i | i <- [0..BS.length st - 1], i `mod` n == j]

transposeAll :: Int -> BS.ByteString -> [BS.ByteString]
transposeAll n st = L.map (\j -> transpose n j st) [0..n-1]

cipherTextBlocks :: IO [BS.ByteString]
cipherTextBlocks = transposeAll <$> mostProbableKeySize <*> secret

guessedKey :: IO BS.ByteString
guessedKey = BS.pack <$> (L.map (\st -> fst . fst $ mostLikelyChar st) <$> cipherTextBlocks)

unSecret :: IO BS.ByteString
unSecret = xorWithKey <$> secret <*> guessedKey

-- This is for practice translating do notation.
challenge6' = guessedKey >>= \key -> unSecret >>= \plainText -> return (key, plainText)

challenge6 :: IO (BS.ByteString, BS.ByteString)
challenge6 = do
    key       <- guessedKey
    plainText <- unSecret
    return (key, plainText)