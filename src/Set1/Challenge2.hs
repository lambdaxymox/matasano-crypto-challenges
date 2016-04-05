module Set1.Challenge2
    (
        challenge2,
        xoredSecret,
    )
    where

import           Util.ByteManipulation
import           Data.Maybe
import qualified Data.ByteString       as BS


xorSecret1 :: BS.ByteString
xorSecret1 = BS.pack $ fromJust $ extractHexBytes "1c0111001f010100061a024b53535009181c"

xorSecret2 :: BS.ByteString
xorSecret2 = BS.pack $ fromJust $ extractHexBytes "686974207468652062756c6c277320657965"

xoredSecret :: BS.ByteString
xoredSecret = BS.pack $ fromJust $ extractHexBytes "746865206b696420646f6e277420706c6179"

-- | Challenge 2
challenge2 = xorSecret1 `xor` xorSecret2
