module Util (
    xorBytes,
    xorBytesShortest,
    randomBytes,
    randomBytesIO,
    randomChoice,
    randomChoiceIO,
    randomlyKeyedCipher,
    randomlyKeyedCipherIO,
    hammingDistance,
    uniqueness,
    parseQueryString,
    buildQueryString,
    mode,
    getOnly,
    expMod,
    hash
) where

import Control.Monad (guard)
import Data.Bits (popCount, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Common as B
import Data.Char (ord)
import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String (fromString, IsString)
import System.Random (getStdRandom, random, randomR, RandomGen)
import Text.Printf (printf)

import Crypto.Cipher.Types (cipherKeySize, cipherInit,
                            Cipher, KeySizeSpecifier(..))
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash (hashWith, HashAlgorithm)
import qualified Data.ByteArray as BA

import ByteFormat (urlEscape)


-- | XOR two ByteStrings of equal length.
xorBytes :: B.ByteString a => a -> a -> a
xorBytes x y
    | (B.length x) /= (B.length y) = error "ByteStrings differ in length"
    | otherwise = xorBytesShortest x y

-- | XOR two ByteStrings, dropping trailing bytes if one is longer.
xorBytesShortest :: B.ByteString a => a -> a -> a
xorBytesShortest x y = B.pack $ B.zipWith xor x y


-- | Generate a random bytestring of the specified length. This is
-- NOT CRYPTOGRAPHICALLY SECURE. (Haskell appears to lack a built-in
-- CSPRNG. TODO: find a library for that.)
randomBytes :: (RandomGen g, B.ByteString a) => Integer -> g -> (a, g)
randomBytes i gen | i < 0 = error "Length negative"
                  | otherwise = (B.pack bytes, gen')
    where
        (bytes, gen') = randomBytes' i gen
        randomBytes' 0 currentGen = ([], currentGen)
        randomBytes' j currentGen = (byte : remainingBytes, finalGen)
            where
                (byte, nextGen) = random currentGen
                (remainingBytes, finalGen) = randomBytes' (j - 1) nextGen


-- | Generate a random bytestring of the specified length.
-- NOT CRYPTOGRAPHICALLY SECURE.
randomBytesIO :: B.ByteString a => Integer -> IO a
randomBytesIO = getStdRandom . randomBytes


-- | Return a random element from a list.
randomChoice :: RandomGen g => [a] -> g -> (a, g)
randomChoice xs gen | null xs = error "Empty list"
                    | otherwise = (xs !! index, gen')
    where (index, gen') = randomR (0, length xs - 1) gen

-- | Return a random element from a list.
randomChoiceIO :: [a] -> IO a
randomChoiceIO = getStdRandom . randomChoice


-- | Generate a randomly keyed block cipher.
-- NOT CRYPTOGRAPHICALLY SECURE.
randomlyKeyedCipher :: (RandomGen g, Cipher a) => g -> (a, g)
randomlyKeyedCipher gen = (cipher, gen')
    where
        key :: BS.ByteString
        KeySizeFixed keyLength = cipherKeySize cipher
        (key, gen') = randomBytes (fromIntegral keyLength) gen
        CryptoPassed cipher = cipherInit key


-- | Generate a randomly keyed block cipher.
-- NOT CRYPTOGRAPHICALLY SECURE.
randomlyKeyedCipherIO :: Cipher a => IO a
randomlyKeyedCipherIO = getStdRandom randomlyKeyedCipher


-- | Compute the Hamming distance (number of differing bits) between
-- two ByteStrings of equal length.
hammingDistance :: B.ByteString a => a -> a -> Integer
hammingDistance x y
    | (B.length x) /= (B.length y) = error "ByteStrings differ in length"
    | otherwise = sum $ map (fromIntegral . popCount) $ B.zipWith xor x y


-- | Return the fraction of list elements that are unique.
uniqueness :: (Ord a) => [a] -> Double
uniqueness xs = uniqueCount / elemCount
    where
        elemCount = fromIntegral $ length xs
        uniqueCount = fromIntegral $ Set.size $ Set.fromList xs


-- | Parse a query string of the form "foo=bar&..."
parseQueryString :: (B.ByteString a, Ord a) => a -> Maybe (Map.Map a a)
parseQueryString str = guard valid >> result
    where
        splits = map splitEq $ splitAmp str
        splitAmp = B.split $ fromIntegral $ ord '&'
        splitEq = B.split $ fromIntegral $ ord '='
        valid = all ((== 2) . length) splits
        result = Just $ Map.fromList $ map (\[k, v] -> (k, v)) splits


-- | Build a query string of the form "foo=bar&...", URL-escaping any
-- '&' and '=' characters in the input
buildQueryString :: (B.ByteString a, IsString a) => Map.Map a a -> a
buildQueryString mapping = joinAmp $ map joinEq splits
    where
        splits = map (\(k, v) -> map urlEscape [k, v]) $ Map.toList mapping
        joinAmp = B.intercalate $ fromString "&"
        joinEq = B.intercalate $ fromString "="


-- | Find the most frequently occuring element of a list.
mode :: Ord a => [a] -> a
mode xs = fst $ maximumBy (compare `on` snd) $ Map.toList counter
    where counter = Map.fromListWith (+) $ zip xs $ repeat 1


-- | Get the single element of list, erroring
-- if the list contains 0 or 2+ elements.
getOnly :: [a] -> a
getOnly [x] = x
getOnly xs = error $ printf "%d elements in list (expected 1)" $ length xs


-- | Calculates (x to the power of y) mod z.
expMod :: Integer -> Integer -> Integer -> Integer
expMod x y z | y < 0 = error "Negative exponent"
             | y == 0 = 1 `mod` z
             | y == 1 = x `mod` z
             | even y = let w = expMod x (y `div` 2) z `mod` z in w * w `mod` z
             | otherwise = x * expMod x (y-1) z `mod` z


-- | Wrapper for hashing something with cryptonite
hash :: (HashAlgorithm h, B.ByteString a) => h -> a -> a
hash alg = B.pack . BA.unpack . hashWith alg . B.toStrict
