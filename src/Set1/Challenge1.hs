import           Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as BSC8 (pack)
import Data.Char (chr)

class Serialize t where
    serialize :: t -> BS.ByteString
    deserialize :: BS.ByteString -> t

instance Serialize String where
    serialize :: String -> BS.ByteString
    serialize = BSC8.pack

    deserialize :: BS.ByteString -> String
    deserialize = map (chr . fromEnum) BS.unpack


base64 :: ByteString -> ByteString
base64 = Base64.encode

