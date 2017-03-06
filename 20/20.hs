import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (for_)
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

    let encodedSecrets = map BC.pack $ lines contents
        Just secrets = sequence $ map base64ToBytes encodedSecrets

        iv = BS.replicate 8 0
        ciphertexts = map (ctrCombine cipher iv) secrets

        minLen = minimum $ map BS.length ciphertexts
        vigenereText = BS.concat $ map (BS.take minLen) ciphertexts

    for_ (V.guessVigenereKey' vigenereText [fromIntegral minLen]) (\sol -> do
            let key = V.key sol
            putStrLn $ "Key: " ++ show key
            sequence_ $ map (print . xorBytesShortest key) ciphertexts
        )

    hClose handle
