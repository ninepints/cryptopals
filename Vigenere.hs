module Vigenere (
    key,
    ciphertext,
    plaintext,
    score,
    showDecryption,
    guessSingleByteKey,
    guessVigenereKey
) where

import Control.Applicative (liftA2)
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B
import Data.Char (chr)
import Data.Function (on)
import Data.List (permutations, sortBy, transpose)
import Data.Word (Word8)

import FrequencyAnalysis
import Util (chunksOf, hammingDistance)

-- | Guesses the key size of the Vigenere cipher used to generate a
-- ciphertext. Looks at key sizes from two to half the length of the
-- ciphertext (up to 64). Returns a list of key sizes, most probable
-- first.
guessKeySize :: B.ByteString -> [Int]
guessKeySize ciphertext = sortBy (compare `on` score) [2..maxSize]
    where
        maxSize = fromIntegral $ min 64 $ div (B.length ciphertext) 2
        score size = (totalDist / fromIntegral (size * length chunkPairs), size)
            where
                isSize = (==) size . fromIntegral . B.length
                chunks = take 4 $ filter isSize $ chunksOf size ciphertext
                chunkPairs = map (take 2) $ permutations chunks
                pairDist [x, y] = hammingDistance x y
                totalDist = fromIntegral $ sum $ map pairDist chunkPairs


data Decryption a = Decryption {
    key :: a,
    ciphertext :: B.ByteString,
    plaintext :: String,
    score :: Double
}

toDecryption :: (B.ByteString -> a -> String) ->
    B.ByteString ->
    a ->
    Decryption a
toDecryption decryptFunc ciphertext key = Decryption
    key
    ciphertext
    (decryptFunc ciphertext key)
    (FrequencyAnalysis.scoreEnglish $ decryptFunc ciphertext key)

-- Builds a String representation of a Decryption. I could just make
-- Decryption part of the Show typeclass, but the Show documentation
-- says Show representations should be "syntactically correct Haskell
-- expressions" and I don't wanna print the ciphertext out each time
showDecryption :: (Show a) => Decryption a -> String
showDecryption d = "Decryption {key = " ++ show (key d) ++
    ", plaintext = " ++ show (plaintext d) ++
    ", score = " ++ show (score d) ++ "}"


chr' :: (Integral a) => a -> Char
chr' = chr . fromIntegral


-- | Guesses the single-byte repeating key with which a ciphertext
-- was XOR'ed. Returns a list of all possible decryptions.
guessSingleByteKey :: B.ByteString -> [Decryption Word8]
guessSingleByteKey ciphertext = map (toDecryption decrypt ciphertext) [0..255]
    where
        decrypt text key = map (chr' . (xor key)) $ B.unpack text


-- | Guesses the Vigenere key with which a ciphertext was XOR'ed.
guessVigenereKey :: B.ByteString -> [Decryption B.ByteString]
guessVigenereKey ciphertext
    | B.length ciphertext < 4 = error "Four-plus bytes required to guess key"
    | otherwise = map (toDecryption decrypt ciphertext) vigenereKeys
    where
        topKeySizes = take 3 $ guessKeySize ciphertext
        decrypt text key = map chr' $ B.zipWith xor (B.cycle key) text

        -- Single-byte ciphertexts for each byte for each key size
        singleByteCiphertexts :: [[B.ByteString]]
        singleByteCiphertexts = map (B.transpose . chunkCiphertext) topKeySizes
            where chunkCiphertext size = chunksOf size ciphertext

        -- Top single-byte keys for each byte for each key size
        -- (currently the very top key because considering multiple
        -- keys leads to a combinatorial explosion if the overall key
        -- is longer than a few bytes)
        singleByteKeys :: [[[Word8]]]
        singleByteKeys = map (map guessKeys) singleByteCiphertexts
            where
                guessKeys = take 1 . map key . sortBy cmp . guessSingleByteKey
                cmp = compare `on` score

        -- Top Vigenere keys for each key size (all combinations of
        -- single-byte keys)
        vigenereKeys :: [B.ByteString]
        vigenereKeys = map B.pack $ concat $ map combineKeys singleByteKeys
            where combineKeys = foldr (liftA2 (:)) [[]]
