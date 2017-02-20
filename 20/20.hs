import qualified Data.ByteString as B
import Data.Foldable (for_)
import Data.String (fromString)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, IOMode(ReadMode), openFile)

import Crypto.Cipher.AES (AES128)

import BlockCipher (ctrCombine)
import ByteFormat (base64ToBytes)
import Util (randomlyKeyedCipherIO, xorBytesShortest)
import qualified Vigenere as V


main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128

    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let encodedSecrets = map fromString $ lines contents
        Just secrets = sequence $ map base64ToBytes encodedSecrets

        iv = B.replicate 8 0
        ciphertexts = map (ctrCombine cipher iv) secrets

        minLen = minimum $ map B.length ciphertexts
        vigenereText = B.concat $ map (B.take minLen) ciphertexts

    for_ (V.guessVigenereKey' vigenereText [fromIntegral minLen]) (\sol -> do
            let key = V.key sol
            putStrLn $ "Key: " ++ show key
            sequence_ $ map (print . xorBytesShortest key) ciphertexts
        )

    hClose handle
