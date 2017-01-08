module Padding (pkcs7pad, pkcs7unpad) where

import Control.Monad (guard)
import qualified Data.ByteString.Common as B


-- | PKCS#7-pad a ByteString to a multiple of some block size.
pkcs7pad :: B.ByteString a => Integer -> a -> a
pkcs7pad blockSize bytes | blockSize < 1 = error "Block size zero or negative"
                         | otherwise = B.append bytes padding
    where
        padLength = blockSize - ((fromIntegral $ B.length bytes) `mod` blockSize)
        padByte = fromIntegral padLength
        padding = B.pack $ replicate (fromIntegral padLength) padByte


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
