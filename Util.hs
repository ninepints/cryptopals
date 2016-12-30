module Util where

import Data.Bits (popCount, xor)

import qualified Data.ByteString.Lazy as B
import Prelude hiding (drop, null, take)
import qualified Prelude as P


class Chunkable a where
    -- Is "null" necessary? The ByteString documentation
    -- doesn't mention pattern matching on "empty"...
    null :: a -> Bool
    empty :: a
    take :: Integral b => b -> a -> a
    drop :: Integral b => b -> a -> a

instance Chunkable [a] where
    null = P.null
    empty = []
    take = P.take . fromIntegral
    drop = P.drop . fromIntegral

instance Chunkable B.ByteString where
    null = B.null
    empty = B.empty
    take = B.take . fromIntegral
    drop = B.drop . fromIntegral


-- | Partition a chunkable object into chunks of uniform size.
-- The final chunk may be smaller than specified.
chunksOf :: (Chunkable a, Integral b) => b -> a -> [a]
chunksOf n xs | n < 1 = error "Chunk size zero or negative"
              | null xs = empty
              | otherwise = take n xs : chunksOf n (drop n xs)


hammingDistance :: B.ByteString -> B.ByteString -> Integer
hammingDistance x y
    | (B.length x) /= (B.length y) = error "ByteStrings differ in length"
    | otherwise = sum $ map (fromIntegral . popCount) $ B.zipWith xor x y

