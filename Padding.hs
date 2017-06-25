module Padding (
    pkcs7pad,
    pkcs7unpad,
    pkcs1_15pad,
    pkcs1_15padIO,
    pkcs1_15unpad,
    constantPad,
    constantPadLeft
) where

import Control.Monad ((<=<), guard)
import qualified Data.ByteString.Common as B
import Data.Word (Word8)
import System.Random (getStdRandom, randomR, RandomGen)

import Util (finiteRandoms)


-- | Given a block size and an input size, how many bytes must we
-- append to the input to make its length a multiple of the block size?
padLength :: Integer -> Integer -> Integer
padLength blockSize byteCount = (blockSize - bytesInLastBlock) `mod` blockSize
    where bytesInLastBlock = byteCount `mod` blockSize


-- | PKCS#7-pad a ByteString to a multiple of some block size.
pkcs7pad :: B.ByteString a => Integer -> a -> a
pkcs7pad blockSize bytes | blockSize < 1 = error "Block size zero or negative"
                         | otherwise = B.append bytes padding
    where
        -- If the input is already a multiple of the block size,
        -- we want a full block of padding
        padLen = (padLength blockSize $ B.length bytes + 1) + 1
        padding = B.replicate padLen (fromIntegral padLen)


-- | Remove PKCS#7 padding from a ByteString.
pkcs7unpad :: B.ByteString a => a -> Maybe a
pkcs7unpad bytes | B.null bytes = Nothing
                 | otherwise = result
    where
        lastByte = B.last bytes
        lastByte' = fromIntegral lastByte
        lastByteValid = lastByte > 0 && lastByte' < B.length bytes
        (msg, padding) = B.splitAt (B.length bytes - lastByte') bytes
        paddingValid = B.all (== lastByte) padding
        result = guard lastByteValid >> guard paddingValid >> Just msg


-- | PKCS#1-v1.5-pad a ByteString to a block of the given size.
pkcs1_15pad :: (B.ByteString a, RandomGen g) => Integer -> a -> g -> (a, g)
pkcs1_15pad blockSize bytes gen
    | blockSize < 1 = error "Block size zero or negative"
    | B.length bytes > blockSize - 11 = error "Message too long for block"
    | otherwise = (B.concat parts, gen')
    where
        padLen = blockSize - B.length bytes - 3
        (padBytes, gen') = finiteRandoms (randomR (1, 255)) padLen gen
        parts = [
                B.pack [0, 2],
                B.pack padBytes,
                B.singleton 0,
                bytes
            ]

-- | PKCS#1-v1.5-pad a ByteString to a block of the given size.
pkcs1_15padIO :: B.ByteString a => Integer -> a -> IO a
pkcs1_15padIO = (fmap . fmap) getStdRandom pkcs1_15pad


-- | Remove PKCS#1 v1.5 padding from a ByteString.
pkcs1_15unpad :: B.ByteString a => a -> Maybe a
pkcs1_15unpad = B.stripPrefix (B.singleton 0) <=<
    return . B.dropWhile (/= 0) <=<
    B.stripPrefix (B.pack [0, 2])


-- | Pad a ByteString to a multiple of some block size
-- using a constant byte.
constantPad :: B.ByteString a => Integer -> Word8 -> a -> a
constantPad = _constantPad False

-- | Pad a ByteString to a multiple of some block size using a
-- constant byte. Padding is appended to the beginning of the string.
constantPadLeft :: B.ByteString a => Integer -> Word8 -> a -> a
constantPadLeft = _constantPad True

_constantPad :: B.ByteString a => Bool -> Integer -> Word8 -> a -> a
_constantPad onLeft blockSize padByte bytes
    | blockSize < 1 = error "Block size zero or negative"
    | otherwise = (if onLeft then flip else id) B.append bytes padding
    where padding = B.replicate (padLength blockSize $ B.length bytes) padByte
