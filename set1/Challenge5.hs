module Challenge5
   where
      
import qualified Data.ByteString as B --(getLine,unpack,pack,ByteString)
import qualified Data.ByteString.Char8 as B8 (unpack, pack)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import Challenge3

repeatingKeyXOR m k = Base16.encode $ fixedXOR m k


solution = do
   m <- B.getContents 
   print $ repeatingKeyXOR m $ B8.pack "ICE"
