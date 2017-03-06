import Control.DeepSeq (deepseq)
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

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
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let Just decodedContents = base64ToBytes $ B.pack $ concat $ lines contents

        cipher :: AES128
        CryptoPassed cipher = cipherInit $ B.pack "YELLOW SUBMARINE"

        plaintext = ecbDecrypt cipher decodedContents

    -- Force Haskell to evaluate the plaintext and read the file before closing
    plaintext `deepseq` hClose handle
    return plaintext


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
