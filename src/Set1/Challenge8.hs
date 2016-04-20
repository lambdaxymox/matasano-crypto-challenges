module Set1.Challenge8
    (
        secrets,
        score,
        guessCipherText,
        challenge8,
    )
    where

import           Util.IO                             (readLines, sizedBlocks)
import           Util.Util                           (right)
import qualified Data.ByteString.Char8               as BSC8
import qualified Data.ByteString                     as BS
import           Data.Function                       (on)
import qualified Data.List                           as L
import           Util.ByteManipulation               (transposeAll)
import           Util.Hexadecimal                    (extractHexBytes)
import           Data.Maybe                          (fromJust)


secrets :: IO [BS.ByteString]
secrets = L.map (BS.pack . extractHexBytes . BSC8.unpack) <$> readLines "Set1/ex8.txt"

score :: BS.ByteString -> Int
score st = L.foldr (\p acc -> acc + same p) 0 $ pairs st
    where
        pairs st = L.map (\l -> (l !! 0, l !! 1)) $ filter(\l -> L.length l == 2) $ L.subsequences $ chunkify st
        chunkify st = right $ sizedBlocks 16 st
        same pair
            | fst pair == snd pair = 1
            | otherwise            = 0

guessCipherText :: [BS.ByteString] -> BS.ByteString
guessCipherText = L.maximumBy (compare `on` score)

-- | Challenge 8
challenge8 :: IO (Int, BS.ByteString)
challenge8 = do
            cipherTexts    <- secrets
            let cipherText = guessCipherText cipherTexts
            let idx        = fromJust $ L.elemIndex cipherText cipherTexts
            return (idx, cipherText)