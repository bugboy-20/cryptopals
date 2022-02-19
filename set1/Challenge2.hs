module Challenge2
   where

import Challenge1

import Data.Bits

import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString as B (zipWith,pack,getLine)


fixedXORIO = do
   a <- readHex
   Base16.encode . fixedXOR a <$> readHex

fixedXOR a b = B.pack $ B.zipWith xor a b
