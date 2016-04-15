module Set1.Challenge1
    (
        challenge1,
        secretBase64,
        secret,
    )
    where

import           Util (extractHexBytes)
import           Util.ByteManipulation
import           Data.Maybe
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC8 (pack)
import qualified Data.ByteString.Base64   as Base64


-- | The secret string as a string of hexadecimal digits.
secret' :: String
secret' = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

-- | The actual string after the hexadecimal has been parsed and packed.
secret :: BS.ByteString
secret = BS.pack $ extractHexBytes secret'

-- | The answer to the puzzle packed into a ByteString.
secretBase64 :: BS.ByteString
secretBase64 = BSC8.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

-- | Challenge 1
challenge1 = Base64.encode secret
