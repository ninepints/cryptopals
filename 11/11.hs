import qualified Data.ByteString as B
import Data.Char (ord)
import System.Random (newStdGen, random, randomR, RandomGen)
import Text.Printf (printf)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, ecbEncrypt)

import BlockCipher (cbcEncrypt)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad)
import Util (randomBytes, randomlyKeyedCipher, uniqueness)


ecbOrCbc :: (RandomGen a) => a -> B.ByteString -> (B.ByteString, Bool, a)
ecbOrCbc gen input = (ciphertext, isCbc, finalGen)
    where
        (prefixLength, gen2) = randomR (5, 10) gen
        (suffixLength, gen3) = randomR (5, 10) gen2
        (prefix, gen4) = randomBytes prefixLength gen3
        (suffix, gen5) = randomBytes suffixLength gen4
        (isCbc, gen6) = random gen5

        blockLength = fromIntegral $ blockSize cipher

        cipher :: AES128
        (cipher, gen7) = randomlyKeyedCipher gen6
        (iv, finalGen) = randomBytes blockLength gen7

        encrypt = if isCbc then cbcEncrypt cipher iv else ecbEncrypt cipher
        plaintext = B.concat [prefix, input, suffix]
        ciphertext = encrypt $ pkcs7pad blockLength plaintext


guessIsCbc :: B.ByteString -> Bool
guessIsCbc bytes = not $ uniqueness (chunksOf 16 bytes) < 1


testGuess :: IO Bool
testGuess = do
    gen <- newStdGen
    let aaa = B.replicate 48 $ fromIntegral $ ord 'A'
        (ciphertext, truth, _) = ecbOrCbc gen aaa
        guess = guessIsCbc ciphertext
    putStrLn $ printf "Guessed %v, truth was %v" (show guess) (show truth)
    return $ truth == guess


main :: IO ()
main = do
    results <- sequence $ replicate 20 testGuess
    let guesses = length results
        failures = length $ filter not results
    putStrLn $ printf "%u guesses with %u failures" guesses failures
