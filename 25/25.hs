import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, cipherInit, ecbDecrypt)
import Crypto.Error (CryptoFailable(..))

import BlockCipher (ctrCombine)
import ByteFormat (base64ToBytes)
import Util (randomBytesIO, randomlyKeyedCipherIO, xorBytes)


-- ECB decryption copied from ex7
getPlaintext :: IO B.ByteString
getPlaintext = do
    [filename] <- getArgs
    contents <- readFile filename

    let Just decodedContents = base64ToBytes $ B.pack $ concat $ lines contents

        cipher :: AES128
        CryptoPassed cipher = cipherInit $ B.pack "YELLOW SUBMARINE"

    return $ ecbDecrypt cipher decodedContents


main :: IO ()
main = do
    plaintext <- getPlaintext
    cipher <- randomlyKeyedCipherIO :: IO AES128
    let ivSize = fromIntegral $ blockSize cipher `div` 2
    iv <- randomBytesIO ivSize :: IO B.ByteString

    let ciphertext = ctrCombine cipher iv plaintext
        aaa = B.replicate (B.length ciphertext) 'A'
        aaa' = ctrCombine cipher iv aaa

    putStrLn "Ciphertext xor encrypted As xor plain As equals..."
    print $ xorBytes ciphertext $ xorBytes aaa aaa'
