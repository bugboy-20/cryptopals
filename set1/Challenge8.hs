module Challenge8
   where
import Challenge1

import Data.List.Split
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B

inp = map (chunksOf 16 . B.unpack . hexToByteString . B8.pack) . lines <$> readFile "./8.txt"

spotRepetitions l = (or $ m l,l)
   where
      m [] = [] -- for each block che if it's repeated
      m (n:l) = s n l : m l
         where
            s e = foldr (\ n -> (||) (e == n)) False --check if the block e is repeated

solve = do
   (b,bl) <- head. filter fst . map spotRepetitions <$> inp
   return $ B.pack . concat $ bl
