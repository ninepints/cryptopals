module Data.ByteString.Common where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Prelude (($), (.), fromIntegral, id, Bool, Integer, Maybe)
import qualified Prelude as P


-- | Operations on strict or lazy ByteStrings.
class ByteString a where
    all :: (Word8 -> Bool) -> a -> Bool
    append :: a -> a -> a
    break :: (Word8 -> Bool) -> a -> (a, a)
    concat :: [a] -> a
    concatMap :: (Word8 -> a) -> a -> a
    cons :: Word8 -> a -> a
    dropWhile :: (Word8 -> Bool) -> a -> a
    elem :: Word8 -> a -> Bool
    empty :: a
    filter :: (Word8 -> Bool) -> a -> a
    foldl :: (b -> Word8 -> b) -> b -> a -> b
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
    stripPrefix :: a -> a -> Maybe a
    tail :: a -> a
    take :: Integer -> a -> a
    toStrict :: a -> BS.ByteString
    transpose :: [a] -> [a]
    unfoldr :: (b -> Maybe (Word8, b)) -> b -> a
    unpack :: a -> [Word8]
    zipWith :: (Word8 -> Word8 -> b) -> a -> a -> [b]

instance ByteString BS.ByteString where
    all = BS.all
    append = BS.append
    break = BS.break
    concat = BS.concat
    concatMap = BS.concatMap
    cons = BS.cons
    dropWhile = BS.dropWhile
    elem = BS.elem
    empty = BS.empty
    filter = BS.filter
    foldl = BS.foldl
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
    stripPrefix = BS.stripPrefix
    tail = BS.tail
    take = BS.take . fromIntegral
    toStrict = id
    transpose = BS.transpose
    unfoldr = BS.unfoldr
    unpack = BS.unpack
    zipWith = BS.zipWith

instance ByteString BL.ByteString where
    all = BL.all
    append = BL.append
    break = BL.break
    concat = BL.concat
    concatMap = BL.concatMap
    cons = BL.cons
    dropWhile = BL.dropWhile
    elem = BL.elem
    empty = BL.empty
    filter = BL.filter
    foldl = BL.foldl
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
    stripPrefix = BL.stripPrefix
    tail = BL.tail
    take = BL.take . fromIntegral
    toStrict = BL.toStrict
    transpose = BL.transpose
    unfoldr = BL.unfoldr
    unpack = BL.unpack
    zipWith = BL.zipWith


-- | Takes the first N bytes of the infinite repetition of a ByteString.
cycleToLength :: ByteString a => Integer -> a -> a
cycleToLength len = pack . P.take (fromIntegral len) . P.cycle . unpack
