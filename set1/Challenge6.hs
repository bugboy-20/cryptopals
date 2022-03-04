module Challenge6
   where

import Data.Bits
import Data.List
import Data.Word

import Challenge3
import Challenge1

import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString as B (getLine,unpack,pack,ByteString)
import qualified Data.ByteString.Char8 as B8 --(getLine,unpack,pack,ByteString)
import Data.List.Split (chunksOf)

hammingDistance :: Bits a => [a] -> [a] -> Int
hammingDistance n m = sum $ zipWith (\ a b -> popCount $ xor a b) n m

readBase64File :: FilePath -> IO B8.ByteString
readBase64File f = let ef = Base64.decode . B8.pack . filter (/='\n') <$> Prelude.readFile f
                    in ef >>= either fail return


guessKeySize :: [Word8] -> [(Int,Int)]
guessKeySize ws = take 3 $ sortBy (\a b -> compare (snd a) (snd b)) $ map (`keySizeValue` ws) [1..40]

keySizeValue :: Int -> [Word8] -> (Int,Int)
keySizeValue n ws = let ps = chunksOf n $ take (4*n) ws
                     in (n, div ( div (sum $ [hammingDistance  x y | x <- ps, y <- ps]) 4) n)

splitAndTranspose :: [Word8] -> [(Int,[[Word8]])]
splitAndTranspose w = map (\(c,hD) ->
   (c, transpose $ chunksOf c w)) $ guessKeySize w
--p5 w = chunksOf (snd $ guessKeySize w) w

cipherChunks :: [(Int,[[Word8]])] -> [(Int,[(Word8, String, Integer)])]
cipherChunks = map (\ (l, cs) ->
   (l, map (singleByteXORcipher . B.pack) cs))
-- p7 w = map (singleByteXORcipher . B.pack) $ p6 w
--
--formatCipherChunks :: [(Int,[(Word8, String, Integer)])] -> [(Int,B.ByteString,String, Integer)]
formatCipherChunks m = map (\(l,w) ->
   fixedXOR m $ B.pack $ map (\(kp,_,_) -> kp
   ) w)

repeatingKeyBreaker :: [(Int,[(Word8, String, Integer)])] -> (B.ByteString, String, Integer)
repeatingKeyBreaker n = snd . maximumBy (\a b -> compare (snd a) (snd b)) $ map mergeChunks n

mergeChunks :: (Int,[(Word8, String, Integer)]) -> (Int,(B8.ByteString, String, Integer))
mergeChunks (n,d) = (n, mergeChunks')
   where
      mergeChunks' = let (ww, ss, ii) = unzip3 d
                      in (B.pack ww, concat . transpose $ ss, sum ii)

solve = do
   m <- readBase64File "6.txt"
   return $ repeatingKeyBreaker . cipherChunks . splitAndTranspose . B.unpack $ m
