module Vigenere (
    showSolution,
    guessSingleByteKey,
    guessVigenereKey,
    guessVigenereKey',
    Solution(..)
) where

import Data.Bits (xor)
import Data.Function (on)
import Data.List (permutations, sortBy)
import Data.Word (Word8)

import qualified Data.ByteString.Common as B
import Data.Chunkable (chunksOf, Chunkable)
import qualified FrequencyAnalysis
import Util (hammingDistance, xorBytes)


-- | Guesses the key size of the Vigenere cipher used to generate a
-- ciphertext. Looks at key sizes from two to half the length of the
-- ciphertext (up to 64). Returns a list of key sizes, most probable
-- first.
guessKeySize :: (B.ByteString a, Chunkable a) => a -> [Integer]
guessKeySize ciphertext = sortBy (compare `on` score) [2..maxSize]
    where
        maxSize = min 64 $ div (B.length ciphertext) 2
        score size = ((fromIntegral totalDist / fromIntegral totalBytes), size)
            where
                isSize = (==) size . B.length
                chunks = filter isSize $ chunksOf size ciphertext
                chunkPairs = map (take 2) $ permutations $ take 4 chunks
                pairDist [x, y] = hammingDistance x y
                totalDist = sum $ map pairDist chunkPairs
                totalBytes = size * fromIntegral (length chunkPairs)


data Solution a k = Solution {
    key :: k,
    ciphertext :: a,
    plaintext :: a,
    score :: Double
}

toSolution :: B.ByteString a => (a -> k -> a) -> a -> k -> Solution a k
toSolution decipherFunc ciphertext key = Solution
    key
    ciphertext
    (decipherFunc ciphertext key)
    (FrequencyAnalysis.scoreEnglish $ decipherFunc ciphertext key)

-- Builds a String representation of a Solution. I could just make
-- Solution part of the Show typeclass, but the Show documentation
-- says Show representations should be "syntactically correct Haskell
-- expressions" and I don't wanna print the ciphertext out each time
showSolution :: (Show a, Show k) => Solution a k -> String
showSolution d = "Solution {key = " ++ show (key d) ++
    ", plaintext = " ++ show (plaintext d) ++
    ", score = " ++ show (score d) ++ "}"


-- | Guesses the single-byte repeating key with which a ciphertext
-- was XOR'ed. Returns a list of all possible solutions.
guessSingleByteKey :: B.ByteString a => a -> [Solution a Word8]
guessSingleByteKey ciphertext = map (toSolution decipher ciphertext) [0..255]
    where decipher text key = B.map (xor key) text



-- | Guesses the Vigenere key with which a ciphertext was XOR'ed.
guessVigenereKey :: (B.ByteString a, Chunkable a) => a -> [Solution a a]
guessVigenereKey ciphertext
    | B.length ciphertext < 4 = error "Four-plus bytes required to guess key"
    | otherwise = guessVigenereKey' ciphertext topKeySizes
    where topKeySizes = take 3 $ guessKeySize ciphertext


-- | Guesses the Vigenere key with which a ciphertext was XOR'ed.
-- Guesses are based on the given list of key sizes.
guessVigenereKey' :: (B.ByteString a, Chunkable a) =>
    a -> [Integer] -> [Solution a a]
guessVigenereKey' ciphertext keySizes = map wrapper vigenereKeys
    where
        wrapper = toSolution decipher ciphertext
        decipher text key = xorBytes (B.cycleToLength (B.length text) key) text

        -- Single-byte ciphertexts for each byte for each key size: [[a]]
        singleByteCiphertexts = map (B.transpose . chunkCiphertext) keySizes
            where chunkCiphertext size = chunksOf (fromIntegral size) ciphertext

        -- Top single-byte keys for each byte for each key size
        -- (currently the very top key because considering multiple
        -- keys leads to a combinatorial explosion if the overall key
        -- is longer than a few bytes): [[[Word8]]]
        singleByteKeys = map (map guessKeys) singleByteCiphertexts
            where
                guessKeys = take 1 . map key . sortBy cmp . guessSingleByteKey
                cmp = compare `on` score

        -- Top Vigenere keys for each key size (all combinations of
        -- single-byte keys): [a]
        vigenereKeys = map B.pack $ concat $ map sequence singleByteKeys
