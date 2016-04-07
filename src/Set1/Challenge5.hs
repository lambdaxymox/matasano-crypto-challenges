module Set1.Challenge5
    (
        challenge5,
        secret,
        secretKey,
    )
    where

import           Util (ToHexadecimal(..))
import           Util.ByteManipulation (xorWithKey)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8

secret :: BS.ByteString
secret = BSC8.pack "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

secretKey :: BS.ByteString
secretKey = BSC8.pack "ICE"

answer :: BS.ByteString
answer = BSC8.pack "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"


-- | Challenge 5
challenge5 :: BS.ByteString
challenge5 = BSC8.pack $ toHex $ secret `xorWithKey` secretKey