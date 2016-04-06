module Util.IO
    (
        readLines,
    )
    where

import qualified Data.ByteString                 as BS
import           Util.ByteManipulation (bslines)
import           System.IO


readLines :: String -> IO [BS.ByteString]
readLines fname = do
    file     <- openFile fname ReadMode
    contents <- BS.hGetContents file
    strings  <- return $ bslines contents
    hClose file 
    return strings