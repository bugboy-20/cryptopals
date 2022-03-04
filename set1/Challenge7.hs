import Crypto.Cipher.AES

import Challenge6
import qualified Data.ByteString.Char8 as B8

key = initAES $ B8.pack "YELLOW SUBMARINE"

solve = decryptECB key <$> readBase64File "./7.txt"
