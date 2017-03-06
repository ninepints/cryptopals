import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, hSetBuffering, openFile, stdin,
                  BufferMode(NoBuffering), IOMode(ReadMode))

import Crypto.Cipher.AES (AES128)

import BlockCipher (ctrCombine)
import ByteFormat (base64ToBytes)
import SharedKeystream (crackSharedKeystream)
import Util (randomlyKeyedCipherIO, xorBytesShortest)


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

    hSetBuffering stdin NoBuffering
    keystream <- crackSharedKeystream stdin ciphertexts
    putStrLn "\ESC[2J"
    putStrLn $ "The key is " ++ show keystream
    putStrLn $ "The plaintexts are:"
    sequence_ $ map (print . xorBytesShortest keystream) ciphertexts

    hClose handle
