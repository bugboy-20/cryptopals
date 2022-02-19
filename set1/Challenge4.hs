module Challenge4
   where

import Challenge3
import Challenge1

import Data.Word

import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString as B (getLine,unpack,pack,ByteString)
import qualified Data.ByteString.Char8 as B8 (getLine,unpack,pack,ByteString)
import Data.ByteString.UTF8 (toString)

readLines = lines <$> readFile "4.txt"

--detectSingleCharacterXOR :: [B.ByteString] -> (Word8, String, Integer)
detectSingleCharacterXOR  = map singleByteXORcipher

solution = Challenge3.maximum . detectSingleCharacterXOR . map B8.pack <$> readLines
