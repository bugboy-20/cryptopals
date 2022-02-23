module Challenge6
   where

import Data.Bits

import Data.Word

import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString as B (getLine,unpack,pack,ByteString)
import qualified Data.ByteString.Char8 as B8 --(getLine,unpack,pack,ByteString)

hammingDistance :: [Word8] -> [Word8] -> Int
hammingDistance n m = sum $ zipWith (\ a b -> popCount $ xor a b) n m

readBase64File :: FilePath -> IO B8.ByteString
readBase64File f = let ef = Base64.decode . B8.pack . filter (/='\n') <$> Prelude.readFile f
                    in ef >>= either fail return


guessKeySize = map keySizeValue [1..40]

keySizeValue :: Int -> [Word8] -> (Int,Int)
keySizeValue n ws = let (a, rr) = splitAt n ws
                        b = take n rr
                     in (n, div (hammingDistance a b) n)
