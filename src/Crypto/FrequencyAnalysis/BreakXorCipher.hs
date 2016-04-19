module Crypto.FrequencyAnalysis.BreakXorCipher
    (
        breakXorCharKeyWith,
        breakXorKeyWith,
        score,
    )
    where

import           Util.ByteManipulation
import qualified Data.ByteString             as BS
import           Data.Map                    as Map
import           Data.Word
import           Crypto.FrequencyAnalysis
import qualified Data.List                   as L
import           Data.Function               (on)


maxCharWith :: (Floating a, Ord a) => (BS.ByteString -> a)
                                      -> [Word8]
                                      -> BS.ByteString 
                                      -> ((Word8, BS.ByteString), a)
maxCharWith scoreFunc charSet st = L.maximumBy (compare `on` snd) scores
    where
        scores = L.map (\ch -> ((ch, st), scoreFunc $ cipherText ch st)) charSet
        cipherText = xorWithChar


minCharWith :: (Floating a, Ord a) => (BS.ByteString -> a)
                                      -> [Word8]
                                      -> BS.ByteString 
                                      -> ((Word8, BS.ByteString), a)
minCharWith scoreFunc charSet st = L.minimumBy (compare `on` snd) scores
    where
        scores = L.map (\ch -> ((ch, st), scoreFunc $ cipherText ch st)) charSet
        cipherText = xorWithChar


-- | The 'scoreFunc' function simply adds up the relative frequency that each character in a string with respect
--   to a statistical model of the plaintext language.
scoreFunc :: (Floating a, Ord a) => Map.Map Word8 a -> Map.Map Word8 a -> a
scoreFunc model = Map.foldrWithKey scoreFunc' 0.0
    where
        scoreFunc' k _ acc = acc + term k

        term k = case Map.lookup k model of
            Nothing -> 0.0
            Just p  -> p

-- | The 'score' function scores a string with respect to the underlying statical model
--   of the langauge of the plaintext.
score :: (Floating a, Ord a) => Map.Map Word8 a -> BS.ByteString -> a
score model = scoreWith $ scoreFunc model 


breakXorCharKeyWith :: (Floating a, Ord a) => Map.Map Word8 a 
                                           -> [Word8] 
                                           -> BS.ByteString 
                                           -> ((Word8, BS.ByteString), a)
breakXorCharKeyWith model charSet st = maxCharWith (score model) charSet st

breakXorKeyWith :: Map.Map Word8 Double -> BS.ByteString -> BS.ByteString
breakXorKeyWith model st = BS.empty