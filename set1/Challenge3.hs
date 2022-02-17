{-# LANGUAGE OverloadedStrings #-}

module Challenge3
   where

import qualified Challenge2
import Challenge1

import Data.Word
import Data.List
import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString as B --(getLine)
import Data.ByteString.UTF8 (toString)


freqList = "etaonihsrlducmwyfgpbvkjxqz"

voto = sum . map (\x -> voto' x freqList 27)
   where
      voto' c [] n = n
      voto' c (m:l) n
        | c == m = n
        | otherwise = voto' c l (n-1)

sameLenth :: B.ByteString -> B.ByteString -> [B.ByteString]
sameLenth r t =
   let a=B.unpack r
       b=B.unpack t
    in map B.pack $ sameLenth' a b
  where
     sameLenth' :: [Word8] -> [Word8] -> [[Word8]]
     sameLenth' a b
       | length a == length b = [a,b]
       | length a > length b = [a, extend b $ length a - length b]
       | length a < length b = [extend a $ length b - length a,b]


extend a 0 = a
extend a n = (head a::Word8) : extend a (n-1)

fixedXOR s1 s2 =
   let (a:b:_) = sameLenth s1 s2
    in Challenge2.fixedXOR a b

singleByteXORcipher = do
   w <- readHex
   let decipher = toString . fixedXOR w . B.pack . singleton
   --p <- map (B.pack . singleton) [0::Word8 .. 255 :: Word8]
   let translations = map (\k -> (k, voto $ decipher k, decipher k)) [0::Word8 .. 255 :: Word8]

   print . maximum' $ translations
      where
         maximum' l = maximum'' (0::Word8,0,"") l
            where
               maximum'' (a,b,c) [] = (a,c)
               maximum'' (a,b,c) ((e,f,g):l) = if b>f then maximum'' (a,b,c) l else maximum'' (e,f,g) l
