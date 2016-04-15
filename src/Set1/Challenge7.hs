module Set1.Challenge7 where
    (
        secret,
        secretKey,
        challenge7,
    )
    where

import Data.ByteString        as BS
import Data.ByteString.Char8  as BSC8
import Data.ByteString.Base64 as Base64


secret :: IO BS.ByteString
secret = Base64.decodeLenient <$> readBS "Set1/ex7.txt"

secretKey :: BS.ByteString
secretKey = BSC8.pack "YELLOW SUBMARINE"

challenge7 :: IO BS.ByteString
challenge7 = return BS.empty