import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (CryptoFailable(..))

import ByteFormat (base64ToBytes)
import BlockCipher (ctrCombine)


ciphertext :: BS.ByteString
Just ciphertext = base64ToBytes $ BC.pack
    "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

key :: BS.ByteString
key = BC.pack "YELLOW SUBMARINE"

iv :: BS.ByteString
iv = BS.replicate 8 0

cipher :: AES128
CryptoPassed cipher = cipherInit key

main :: IO ()
main = print $ ctrCombine cipher iv ciphertext
