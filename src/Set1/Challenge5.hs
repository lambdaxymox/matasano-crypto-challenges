module Set1.Challenge5
    (
        challenge5,
        secret,
        secretKey,
    )
    where

import           Util.ByteManipulation (xorWithKey, hexBS, ToHexadecimal(..))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8

secret :: BS.ByteString
secret = BSC8.pack "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

secretKey :: BS.ByteString
secretKey = BSC8.pack "ICE"

answer :: BS.ByteString
answer = secret `xorWithKey` secretKey

challenge5 :: BS.ByteString
challenge5 = BSC8.pack $ toHex answer