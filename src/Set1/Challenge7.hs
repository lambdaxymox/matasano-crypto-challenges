module Set1.Challenge7
    (
        secret,
        secretKey,
        cipher,
        challenge7,
    )
    where

import           Util.IO                (readBS)
import           Data.ByteString        as BS
import           Data.ByteString.Char8  as BSC8
import           Data.ByteString.Base64 as Base64
import qualified Crypto.Cipher.AES      as AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Maybe


secret :: IO BS.ByteString
secret = Base64.decodeLenient <$> readBS "Set1/ex7.txt"

secretKey :: BS.ByteString
secretKey = BSC8.pack "YELLOW SUBMARINE"

cipher :: AES.AES128
cipher = fromJust $ maybeCryptoError $ cipherInit secretKey

challenge7 :: IO BS.ByteString
challenge7 = ecbDecrypt cipher <$> secret