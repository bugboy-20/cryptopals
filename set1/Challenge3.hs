module Challenge3
   where

import qualified Challenge2
import Challenge1
import EnglishDetector

import Data.Word
import qualified Data.Map.Strict as M
import Data.List as L
import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString as B (getLine,unpack,pack,ByteString)
import qualified Data.ByteString.Char8 as B8 (unpack)


sameLenth :: B.ByteString -> B.ByteString -> [B.ByteString]
sameLenth r t =
   let a=B.unpack r
       b=B.unpack t
    in map B.pack $ sameLenth' a b
  where
     sameLenth' :: [Word8] -> [Word8] -> [[Word8]]
     sameLenth' a b
       | length a == length b = [a,b]
       | length a > length b = [a, resize b $ length a]
       | length a < length b = [resize a $ length b, b]


extend a 0 = a
extend a n = (head a::Word8) : extend a (n-1)

resize a n
  | length a > n = take n a
  | otherwise = a ++ resize a (n-length a)


fixedXOR s1 s2 =
   let (a:b:_) = sameLenth s1 s2
    in Challenge2.fixedXOR a b

singleByteXORcipher w =
   let decipher = B8.unpack . fixedXOR w . B.pack . singleton
   --p <- map (B.pack . singleton) [0::Word8 .. 255 :: Word8]
       translations = map (\k -> (k, decipher k, voto $ decipher k)) [0::Word8 .. 255 :: Word8]

   in Challenge3.maximum translations

maximum l = maximum'' (0::Word8,"",0) l
   where
      maximum'' (a,b,c) [] = (a,b,c)
      maximum'' (a,b,c) ((e,f,g):l) = if c>g then maximum'' (a,b,c) l else maximum'' (e,f,g) l

-- solution = singleByteXORcipher <$> readHex
