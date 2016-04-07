module Set1.Challenge6
    (
        secret,
        blocks,
        mostProbableKeySize,
        ctChunks,

    )
    where

import           Util.Util             (right)
import           Util.IO               (getKPaddedBlocks, readBS, sizedBlocks)
import           Util.ByteManipulation (meanHammingFracDist, maybeMeanHammingFracDist)
import qualified Data.ByteString       as BS
import           Data.Maybe            (fromJust)
import qualified Data.List             as L (minimumBy)
import           System.IO.Unsafe      (unsafePerformIO)


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
keySizes = [2..40]


mostProbableKeySize :: IO Int
mostProbableKeySize = guessKeySize4 keySizes <$> secret

ctChunks :: IO [BS.ByteString]
ctChunks = fmap (right) $ sizedBlocks <$> mostProbableKeySize <*> secret

