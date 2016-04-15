module Set1.Challenge6
    (
        secret,
        mostProbableKeySize,
        guessedKey,
        cipherTextBlocks,
        guessKeySize,
        guessKeySizeN,
        keySizes,
        challenge6,

    )
    where

import           Crypto.FrequencyAnalysis.English (mostLikelyWord8)
import           Util.Util                        (right)
import           Util.IO                          (getKPaddedBlocks, readBS, sizedBlocks, unpaddedBlockCount)
import           Util.ByteManipulation            (xorWithKey, meanHammingFracDist, transposeAll)
import qualified Data.ByteString                  as BS
import qualified Data.List                        as L (minimumBy, map)
import           Data.Monoid
import           Data.Function                    (on)
import           Data.ByteString.Base64           as Base64
import           Data.ByteString.Char8            as BSC8


secret :: IO BS.ByteString
secret = Base64.decodeLenient <$> readBS "Set1/ex6.txt"

blocks :: Int -> Int -> BS.ByteString -> [BS.ByteString]
blocks k keySize bs = right $ getKPaddedBlocks k keySize bs

guessKeySizeN :: Int -> [Int] -> BS.ByteString -> Int
guessKeySizeN n keySizes bs = L.minimumBy (compare `on` fracDist) keySizes
    where
        fracDist keySize = meanHammingFracDist $ blocks n keySize bs

guessKeySize :: [Int] -> BS.ByteString -> Int
guessKeySize = guessKeySizeN 10

keySizes :: [Int]
keySizes = [2..40]

mostProbableKeySize :: IO Int
mostProbableKeySize = guessKeySize keySizes <$> secret

cipherTextBlocks :: IO [BS.ByteString]
cipherTextBlocks = transposeAll <$> mostProbableKeySize <*> secret

guessedKey :: IO BS.ByteString
guessedKey = BS.pack <$> (L.map (fst . fst . mostLikelyWord8) <$> cipherTextBlocks)

unSecret :: IO BS.ByteString
unSecret = xorWithKey <$> secret <*> guessedKey

answerKey :: BS.ByteString
answerKey = BSC8.pack "Terminator X: Bring the noise"

-- | Challenge 6

-- This is just practice translating do notation. It is equivalent to challenge6.
challenge6' = guessedKey >>= \key -> unSecret >>= \plainText -> return (key, plainText)

challenge6 :: IO (BS.ByteString, BS.ByteString)
challenge6 = do
    key       <- guessedKey
    plainText <- unSecret
    return (key, plainText)