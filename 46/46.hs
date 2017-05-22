import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import ByteFormat (base64ToBytes, integerToBytes, bytesToInteger)
import Util (expMod, modInv, randomPrimeIO)


secret :: B.ByteString
Just secret = base64ToBytes $ BC.pack $
    "VGhhdCdzIHdoeSBJIGZvdW5kIHlvdSBkb24ndCBwbGF5IG" ++
    "Fyb3VuZCB3aXRoIHRoZSBGdW5reSBDb2xkIE1lZGluYQ=="


e :: Integer
e = 3


plaintextOdd :: Integer -> Integer -> Integer -> Bool
plaintextOdd n d ciphertext = odd $ B.last $ integerToBytes $
    expMod ciphertext d n


main :: IO ()
main = do
    (p, q) <- (,) <$> randomPrimeIO 512 <*> randomPrimeIO 512

    let n = p * q
        et = (p-1) * (q-1)
        d = modInv e et
        ciphertext = expMod (bytesToInteger secret) e n

    findPlaintext n (plaintextOdd n d) ciphertext


-- On each iteration, multiply the ciphertext by 2^(iter count) and
-- check whether the plaintext was odd. If it was, move the lower bound
-- up to the midpoint; otherwise, move the upper bound down. We define
-- the midpoint using half-even rounding to avoid accumulated bias
-- (but even so the last byte is slightly off)
findPlaintext :: Integer -> (Integer -> Bool) -> Integer -> IO ()
findPlaintext n oracle ciphertext = find 1 (0, n)
    where find bytesIn (lowBound, highBound) = let
            highBoundBytes = integerToBytes highBound :: B.ByteString
            total = lowBound + highBound
            avg = total `div` 2
            midpoint = avg + if odd total && odd avg then 1 else 0
            newBounds = if oracle $ ciphertext * expMod (2^bytesIn) e n
                then (midpoint, highBound)
                else (lowBound, midpoint)
            in do
                putStrLn $ "Upper bound is " ++ show highBoundBytes
                when (highBound > lowBound + 1) $ find (bytesIn + 1) newBounds
