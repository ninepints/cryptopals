import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, cipherInit)
import Crypto.Error (CryptoFailable(..))

import ByteFormat (base64ToBytes)
import BlockCipher (cbcDecrypt)


main :: IO ()
main = do
    [filename] <- getArgs
    contents <- readFile filename

    let joinedContents = concat $ lines contents
        Just decodedContents = base64ToBytes $ BC.pack $ joinedContents
        key = BC.pack "YELLOW SUBMARINE"
        iv = BS.replicate (blockSize cipher) 0

        cipher :: AES128
        CryptoPassed cipher = cipherInit key
        plaintext = cbcDecrypt cipher iv decodedContents

    putStrLn $ BC.unpack plaintext
