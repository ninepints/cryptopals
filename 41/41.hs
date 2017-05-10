import qualified Data.ByteString.Char8 as B
import System.Random (randomRIO)

import ByteFormat (integerToBytes, bytesToInteger)
import Util (expMod, modInv, randomPrimeIO)


e :: Integer
e = 3


main :: IO ()
main = do
    (p, q) <- (,) <$> randomPrimeIO 1024 <*> randomPrimeIO 1024

    let n = p * q
        et = (p-1) * (q-1)
        d = modInv e et

    s <- randomRIO (2, n-1)

    let ciphertext = expMod (bytesToInteger $ B.pack "the message") e n
        plaintext = expMod ciphertext d n
        plaintextBytes = integerToBytes plaintext :: B.ByteString

        ciphertextTimesS = (expMod s e n * ciphertext) `mod` n
        plaintextTimesS = expMod ciphertextTimesS d n
        plaintext' = (plaintextTimesS * modInv s n) `mod` n
        plaintextBytes' = integerToBytes plaintext' :: B.ByteString

    putStrLn $ "Original ciphertext: " ++ show ciphertext
    putStrLn $ "Modified ciphertext: " ++ show ciphertextTimesS
    putStrLn $ "Original plaintext: " ++ show plaintext
    putStrLn $ "Modified plaintext: " ++ show plaintextTimesS
    putStrLn $ "Restored plaintext: " ++ show plaintext'
    putStrLn $ "Original plaintext bytes: " ++ show plaintextBytes
    putStrLn $ "Restored plaintext bytes: " ++ show plaintextBytes'
