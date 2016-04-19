module Set1.Challenge8
    (
        secrets,
        guessedKey,
        cipherTextBlocks,
        guessKeySize,
        guessKey,
        guessKeySizeN,
        guessedKeySize,
        guessedKey,
        guessedCipherText,
        guessedPlainText,
        challenge8,
    )
    where

import           Util.IO                             (getKPaddedBlocks, readLines, blocks)
import           Crypto.FrequencyAnalysis.English    (mostLikelyWord8C, mostLikelyWord8, cipherScore, score)
import qualified Data.List                           as L
import qualified Data.ByteString.Char8               as BSC8
import           Data.ByteString.Base64              as Base64
import qualified Data.ByteString                     as BS
import           Data.Function                       (on)
import qualified Data.List                           as L (maximumBy, minimumBy, map)
import           Util.ByteManipulation               (meanHammingFracDist, transposeAll)
import qualified Crypto.Cipher.AES                   as AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Maybe
import           Util.Hexadecimal                    (extractHexBytes)


secrets :: IO [BS.ByteString]
secrets = L.map (BS.pack . extractHexBytes . BSC8.unpack) <$> readLines "Set1/ex8.txt"

guessKeySizeN :: Int -> [Int] -> BS.ByteString -> Int
guessKeySizeN n keySizes bs = L.minimumBy (compare `on` fracDist) keySizes
    where
        fracDist keySize = meanHammingFracDist $ blocks n keySize bs

guessKeySize :: [Int] -> BS.ByteString -> Int
guessKeySize = guessKeySizeN 8

guessKey :: BS.ByteString -> BS.ByteString
guessKey = BS.pack . L.map (fst . fst . mostLikelyWord8) . cipherTextBlocks

guessCipherText :: [BS.ByteString] -> BS.ByteString
guessCipherText strings = L.maximumBy (compare `on` score) strings

guessedKeySize :: Int
guessedKeySize = 16

cipherTextBlocks :: BS.ByteString -> [BS.ByteString]
cipherTextBlocks st = transposeAll guessedKeySize st

guessedKey :: IO BS.ByteString
guessedKey = guessKey <$> guessedCipherText

guessedCipherText :: IO BS.ByteString
guessedCipherText = guessCipherText <$> secrets

guessedPlainText :: IO BS.ByteString
guessedPlainText = ecbDecrypt <$> cipher <*> guessedCipherText

cipher :: IO AES.AES128
cipher = fromJust . maybeCryptoError . cipherInit <$> guessedKey

-- | Challenge 8
challenge8 :: IO (BS.ByteString, BS.ByteString)
challenge8 = do
    key       <- guessedKey
    plainText <- guessedPlainText
    return (key, plainText)