module Data.ByteString.Common where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Prelude hiding (length, zipWith)


class ByteString a where
    all :: (Word8 -> Bool) -> a -> Bool
    append :: a -> a -> a
    concatMap :: (Word8 -> a) -> a -> a
    elem :: Word8 -> a -> Bool
    empty :: a
    intercalate :: a -> [a] -> a
    last :: a -> Word8
    length :: a -> Integer
    null :: a -> Bool
    pack :: [Word8] -> a
    singleton :: Word8 -> a
    split :: Word8 -> a -> [a]
    splitAt :: Integer -> a -> (a, a)
    unpack :: a -> [Word8]
    zipWith :: (Word8 -> Word8 -> b) -> a -> a -> [b]

instance ByteString BS.ByteString where
    all = BS.all
    append = BS.append
    concatMap = BS.concatMap
    elem = BS.elem
    empty = BS.empty
    intercalate = BS.intercalate
    last = BS.last
    length = fromIntegral . BS.length
    null = BS.null
    pack = BS.pack
    singleton = BS.singleton
    split = BS.split
    splitAt = BS.splitAt . fromIntegral
    unpack = BS.unpack
    zipWith = BS.zipWith

instance ByteString BL.ByteString where
    all = BL.all
    append = BL.append
    concatMap = BL.concatMap
    elem = BL.elem
    empty = BL.empty
    intercalate = BL.intercalate
    last = BL.last
    length = fromIntegral . BL.length
    null = BL.null
    pack = BL.pack
    singleton = BL.singleton
    split = BL.split
    splitAt = BL.splitAt . fromIntegral
    unpack = BL.unpack
    zipWith = BL.zipWith
