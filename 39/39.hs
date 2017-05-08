import qualified Data.ByteString.Char8 as B

import ByteFormat (integerToBytes, bytesToInteger)
import Util (expMod, modInv, randomPrimeIO)


e :: Integer
e = 3


main :: IO ()
main = do
    -- We can't use very long messages here or they'll be mangled when
    -- we take the modulus with respect to the product of small primes
    testRsa 71 77 $ B.pack "a"

    (p, q) <- (,) <$> randomPrimeIO 1024 <*> randomPrimeIO 1024
    testRsa p q $ B.pack "A longer message!"


testRsa :: Integer -> Integer -> B.ByteString -> IO ()
testRsa p q msg = let
    n = p * q
    et = (p-1) * (q-1)
    d = modInv e et

    ciphertext = expMod (bytesToInteger msg) e n
    ciphertextBytes = integerToBytes ciphertext :: B.ByteString
    plaintext = integerToBytes $ expMod ciphertext d n :: B.ByteString

    in do
        putStrLn $ "Parameters (p, q): " ++ show (p, q)
        putStrLn $ "Plaintext: " ++ show msg
        putStrLn $ "Encrypted: " ++ show ciphertextBytes
        putStrLn $ "Decrypted: " ++ show plaintext

