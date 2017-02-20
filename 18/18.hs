import qualified Data.ByteString as B
import Data.String (fromString)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (CryptoFailable(..))

import qualified ByteFormat
import BlockCipher (ctrCombine)


ciphertext :: B.ByteString
Just ciphertext = ByteFormat.base64ToBytes $ fromString
    "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

key :: B.ByteString
key = fromString "YELLOW SUBMARINE"

iv :: B.ByteString
iv = B.replicate 8 0

cipher :: AES128
CryptoPassed cipher = cipherInit key

main :: IO ()
main = print $ ctrCombine cipher iv ciphertext
