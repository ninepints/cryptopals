module Padding (pkcs7pad, pkcs7unpad, constantPad) where

import Control.Monad (guard)
import qualified Data.ByteString.Common as B
import Data.Word (Word8)


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


-- | Pad a ByteString to a multiple of some block size
-- using a constant byte.
constantPad :: B.ByteString a => Integer -> Word8 -> a -> a
constantPad blockSize padByte bytes
    | blockSize < 1 = error "Block size zero or negative"
    | otherwise = B.append bytes padding
    where padding = B.replicate (padLength blockSize $ B.length bytes) padByte
