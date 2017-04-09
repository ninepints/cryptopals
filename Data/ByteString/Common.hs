module Data.ByteString.Common where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Prelude (($), (.), fromIntegral, id, Bool, Integer)
import qualified Prelude as P


class ByteString a where
    all :: (Word8 -> Bool) -> a -> Bool
    append :: a -> a -> a
    concat :: [a] -> a
    concatMap :: (Word8 -> a) -> a -> a
    cons :: Word8 -> a -> a
    elem :: Word8 -> a -> Bool
    empty :: a
    filter :: (Word8 -> Bool) -> a -> a
    fromStrict :: BS.ByteString -> a
    head :: a -> Word8
    index :: a -> Integer -> Word8
    intercalate :: a -> [a] -> a
    last :: a -> Word8
    length :: a -> Integer
    map :: (Word8 -> Word8) -> a -> a
    null :: a -> Bool
    pack :: [Word8] -> a
    replicate :: Integer -> Word8 -> a
    reverse :: a -> a
    singleton :: Word8 -> a
    split :: Word8 -> a -> [a]
    splitAt :: Integer -> a -> (a, a)
    tail :: a -> a
    take :: Integer -> a -> a
    toStrict :: a -> BS.ByteString
    transpose :: [a] -> [a]
    unpack :: a -> [Word8]
    zipWith :: (Word8 -> Word8 -> b) -> a -> a -> [b]

instance ByteString BS.ByteString where
    all = BS.all
    append = BS.append
    concat = BS.concat
    concatMap = BS.concatMap
    cons = BS.cons
    elem = BS.elem
    empty = BS.empty
    filter = BS.filter
    fromStrict = id
    head = BS.head
    index a i = BS.index a $ fromIntegral i
    intercalate = BS.intercalate
    last = BS.last
    length = fromIntegral . BS.length
    map = BS.map
    null = BS.null
    pack = BS.pack
    replicate = BS.replicate . fromIntegral
    reverse = BS.reverse
    singleton = BS.singleton
    split = BS.split
    splitAt = BS.splitAt . fromIntegral
    tail = BS.tail
    take = BS.take . fromIntegral
    toStrict = id
    transpose = BS.transpose
    unpack = BS.unpack
    zipWith = BS.zipWith

instance ByteString BL.ByteString where
    all = BL.all
    append = BL.append
    concat = BL.concat
    concatMap = BL.concatMap
    cons = BL.cons
    elem = BL.elem
    empty = BL.empty
    filter = BL.filter
    fromStrict = BL.fromStrict
    head = BL.head
    index a i = BL.index a $ fromIntegral i
    intercalate = BL.intercalate
    last = BL.last
    length = fromIntegral . BL.length
    map = BL.map
    null = BL.null
    pack = BL.pack
    replicate = BL.replicate . fromIntegral
    reverse = BL.reverse
    singleton = BL.singleton
    split = BL.split
    splitAt = BL.splitAt . fromIntegral
    tail = BL.tail
    take = BL.take . fromIntegral
    toStrict = BL.toStrict
    transpose = BL.transpose
    unpack = BL.unpack
    zipWith = BL.zipWith


cycleToLength :: ByteString a => Integer -> a -> a
cycleToLength len = pack . P.take (fromIntegral len) . P.cycle . unpack
