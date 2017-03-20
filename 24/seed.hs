import qualified Data.ByteString.Char8 as B
import Data.Word (Word16)
import System.Random (randomIO, randomRIO)

import SpecImplementations.Mersenne (sketchyCtr)
import Util (randomBytesIO)


encrypt :: Word16 -> B.ByteString -> IO B.ByteString
encrypt seed input = do
    prefixLength <- randomRIO (10, 20)
    prefix <- randomBytesIO prefixLength
    return $ sketchyCtr (fromIntegral seed) $ B.append prefix input


main :: IO ()
main = do
    seed <- randomIO :: IO Word16

    let plaintext = B.pack "AAAAAAAAAAAAAA"

    ciphertext <- encrypt seed plaintext

    putStrLn $ "The seed is " ++ show seed
    putStrLn $ "The ciphertext is " ++ show ciphertext
