module Set1.Challenge2
    (
        challenge2,
        secret,
    )
    where

import           Util (extractHexBytes)
import           Util.ByteManipulation
import qualified Data.ByteString       as BS


secret1 :: BS.ByteString
secret1 = BS.pack $ extractHexBytes "1c0111001f010100061a024b53535009181c"

secret2 :: BS.ByteString
secret2 = BS.pack $ extractHexBytes "686974207468652062756c6c277320657965"

secret :: BS.ByteString
secret = BS.pack $ extractHexBytes "746865206b696420646f6e277420706c6179"

-- | Challenge 2
challenge2 :: BS.ByteString
challenge2 = secret1 `xor` secret2
