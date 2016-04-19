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
import           Util.IO                          (readBS, sizedBlocks, unpaddedBlockCount, blocks)
import           Util.ByteManipulation            (xorWithKey, meanHammingFracDist, transposeAll)
import qualified Data.ByteString                  as BS
import qualified Data.List                        as L (minimumBy, map)
import           Data.Monoid
import           Data.Function                    (on)
import           Data.ByteString.Base64           as Base64
import           Data.ByteString.Char8            as BSC8


secret :: IO BS.ByteString
secret = Base64.decodeLenient <$> readBS "Set1/ex6.txt"

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

guessedPlainText :: IO BS.ByteString
guessedPlainText = xorWithKey <$> secret <*> guessedKey

-- | Challenge 6

-- This is just practice translating do notation. It is equivalent to challenge6.
challenge6' = guessedKey >>= \key -> guessedPlainText >>= \plainText -> return (key, plainText)

challenge6 :: IO (BS.ByteString, BS.ByteString)
challenge6 = do
    key       <- guessedKey
    plainText <- guessedPlainText
    return (key, plainText)