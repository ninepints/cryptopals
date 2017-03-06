import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit, ecbDecrypt)
import Crypto.Error (CryptoFailable(..))

import ByteFormat (base64ToBytes)


main :: IO ()
main = do
    [filename] <- getArgs
    contents <- readFile filename

    let Just decodedContents = base64ToBytes $ B.pack $ concat $ lines contents
        key = B.pack "YELLOW SUBMARINE"

        cipher :: AES128
        CryptoPassed cipher = cipherInit key
        plaintext = ecbDecrypt cipher decodedContents

    putStrLn $ B.unpack plaintext
