module Data.Chunkable where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (concat, drop, null, take)
import qualified Prelude as P


class Chunkable a where
    -- Is "null" necessary? The ByteString documentation
    -- doesn't mention pattern matching on "empty"...
    null :: a -> Bool
    empty :: a
    take :: Integer -> a -> a
    drop :: Integer -> a -> a
    concat :: [a] -> a

instance Chunkable [a] where
    null = P.null
    empty = []
    take = P.take . fromIntegral
    drop = P.drop . fromIntegral
    concat = P.concat

instance Chunkable BL.ByteString where
    null = BL.null
    empty = BL.empty
    take = BL.take . fromIntegral
    drop = BL.drop . fromIntegral
    concat = BL.concat

instance Chunkable BS.ByteString where
    null = BS.null
    empty = BS.empty
    take = BS.take . fromIntegral
    drop = BS.drop . fromIntegral
    concat = BS.concat


-- | Partition a chunkable object into chunks of uniform size.
-- The final chunk may be smaller than specified.
chunksOf :: (Chunkable a) => Integer -> a -> [a]
chunksOf n xs | n < 1 = error "Chunk size zero or negative"
              | null xs = empty
              | otherwise = take n xs : chunksOf n (drop n xs)
