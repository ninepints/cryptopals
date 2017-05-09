import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import Math.NumberTheory.Powers (exactCubeRoot)

import ByteFormat (integerToBytes, bytesToInteger)
import Util (expMod, modInv, randomPrimeIO)


e :: Integer
e = 3


main :: IO ()
main = do
    let encryptMsg = encrypt $ B.pack "the message"
    (c1, n1) <- encryptMsg
    (c2, n2) <- encryptMsg
    (c3, n3) <- encryptMsg

    let (n12, n23, n13, n123) = (n1 * n2, n2 * n3, n1 * n3, n1 * n2 * n3)
        plaintextCubed = (c1 * n23 * modInv n23 n1 +
                          c2 * n13 * modInv n13 n2 +
                          c3 * n12 * modInv n12 n3) `mod` n123

        plaintext :: B.ByteString
        plaintext = integerToBytes $ fromJust $ exactCubeRoot plaintextCubed

    putStrLn $ "Recovered message: "  ++ show plaintext


encrypt :: B.ByteString -> IO (Integer, Integer)
encrypt msg = do
    (p, q) <- (,) <$> randomPrimeIO 1024 <*> randomPrimeIO 1024
    let n = p * q
        ciphertext = expMod (bytesToInteger msg) e n
    return (ciphertext, n)
