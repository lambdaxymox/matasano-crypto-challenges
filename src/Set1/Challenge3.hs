module Set1.Challenge3
    (
        challenge3,
        candidates,
        secret,
        answer,
    )
    where

import           Util                                 (extractHexBytes)
import           Util.ByteManipulation
import           Crypto.FrequencyAnalysis.English
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BSC8
import qualified Data.Map.Strict                      as Map
import           Data.List                            (maximumBy, minimumBy)
import           Data.Function                        (on)


secret :: BS.ByteString
secret = BS.pack $ extractHexBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

candidates :: Map.Map Char BS.ByteString
candidates = Map.fromList $ map (\ch -> (ch, secret `xor` repChar ch (BS.length secret))) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

answerS :: BS.ByteString
answerS = BSC8.pack "Cooking MC's like a pound of bacon"

answer :: (Char, BS.ByteString)
answer = ('X', answerS)

-- | Challenge 3
challenge3 :: (Char, BS.ByteString)
challenge3 = (bestChar, bestString)
    where
        scores     = Map.map score candidates
        bestChar   = fst $ maximumBy (compare `on` snd) $ Map.toList scores 
        bestString = secret `xor` repChar bestChar (BS.length secret)

