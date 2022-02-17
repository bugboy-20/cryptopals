module Challenge1
   where

import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString.Char8 as B (getLine)

hexToByteString str =
   let Right s = Base16.decode str
    in s

hexToBase64 = Base64.encode . hexToByteString

readHex = do
   h <- Base16.decode <$> B.getLine
   case h of
     Left err -> fail err
     Right bs -> return bs


hexToBase64IO = Base64.encode <$> readHex

-- solution = hexToBase64IO
