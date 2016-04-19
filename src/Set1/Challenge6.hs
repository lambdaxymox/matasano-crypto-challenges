module Set1.Challenge6
    (
        secret,
        guessedKeySize,
        guessedKey,
        guessKeySize,
        keySizes,
        challenge6,

    )
    where

import           Crypto.FrequencyAnalysis.English        (mostLikelyXorKey)
import           Crypto.FrequencyAnalysis.BreakXorCipher (guessKeySizeN)
import           Util.IO                                 (readBS, blocks)
import           Util.ByteManipulation                   (xorWithKey)
import qualified Data.ByteString                         as BS
import qualified Data.List                               as L (minimumBy, map)
import           Data.Monoid
import           Data.Function                           (on)
import           Data.ByteString.Base64                  as Base64
import           Data.ByteString.Char8                   as BSC8


secret :: IO BS.ByteString
secret = Base64.decodeLenient <$> readBS "Set1/ex6.txt"

guessKeySize :: [Int] -> BS.ByteString -> Int
guessKeySize = guessKeySizeN 10

keySizes :: [Int]
keySizes = [2..40]

guessedKeySize :: IO Int
guessedKeySize = guessKeySize keySizes <$> secret

guessedKey :: IO BS.ByteString
guessedKey = mostLikelyXorKey <$> guessedKeySize <*> secret

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