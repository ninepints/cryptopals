module Data.Chunkable where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (drop, null, take)
import qualified Prelude as P


class Chunkable a where
    null :: a -> Bool
    take :: Integer -> a -> a
    drop :: Integer -> a -> a

instance Chunkable [a] where
    null = P.null
    take = P.take . fromIntegral
    drop = P.drop . fromIntegral

instance Chunkable BL.ByteString where
    null = BL.null
    take = BL.take . fromIntegral
    drop = BL.drop . fromIntegral

instance Chunkable BS.ByteString where
    null = BS.null
    take = BS.take . fromIntegral
    drop = BS.drop . fromIntegral


-- | Partition a chunkable object into chunks of uniform size.
-- The final chunk may be smaller than specified.
chunksOf :: (Chunkable a) => Integer -> a -> [a]
chunksOf n xs | n < 1 = error "Chunk size zero or negative"
              | null xs = []
              | otherwise = take n xs : chunksOf n (drop n xs)
