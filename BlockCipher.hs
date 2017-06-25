module BlockCipher (cbcEncrypt, cbcDecrypt, ctrCombine) where

import qualified Data.ByteString as B

import Crypto.Cipher.Types (blockSize, ecbEncrypt, ecbDecrypt, BlockCipher)

import ByteFormat (integerToBytes)
import qualified Data.Chunkable as C
import Padding (constantPad)
import Util (xorBytes)


-- | Adapts a function that accepts lists of blocks to accept a
-- single ByteString.
blockAdapt :: BlockCipher c =>
    (c -> B.ByteString -> [B.ByteString] -> [B.ByteString]) ->
    c -> B.ByteString -> B.ByteString -> B.ByteString
blockAdapt blockFunc cipher iv = B.concat . blockFunc cipher iv . split
    where split = C.chunksOf $ fromIntegral $ blockSize cipher


-- | Encrypts a ByteString using CBC mode.
-- The input string should be a multiple of the cipher block size.
cbcEncrypt :: BlockCipher c =>
    c -> B.ByteString -> B.ByteString -> B.ByteString
cbcEncrypt = blockAdapt cbcEncryptBlocks

cbcEncryptBlocks :: BlockCipher c =>
    c -> B.ByteString -> [B.ByteString] -> [B.ByteString]
cbcEncryptBlocks  _ _ [] = []
cbcEncryptBlocks cipher iv remainingBlocks = outputHead : outputTail
    where
        outputHead = ecbEncrypt cipher $ xorBytes iv $ head remainingBlocks
        nextIv = outputHead
        outputTail = cbcEncryptBlocks cipher nextIv $ tail remainingBlocks


-- | Decrypts a ByteString using CBC mode.
-- The input string should be a multiple of the cipher block size.
cbcDecrypt :: BlockCipher c =>
    c -> B.ByteString -> B.ByteString -> B.ByteString
cbcDecrypt = blockAdapt cbcDecryptBlocks

cbcDecryptBlocks :: BlockCipher c =>
    c -> B.ByteString -> [B.ByteString] -> [B.ByteString]
cbcDecryptBlocks  _ _ [] = []
cbcDecryptBlocks cipher iv remainingBlocks = outputHead : outputTail
    where
        outputHead = xorBytes iv $ ecbDecrypt cipher $ head remainingBlocks
        nextIv = head remainingBlocks
        outputTail = cbcDecryptBlocks cipher nextIv $ tail remainingBlocks


-- Encrypts or decrypts a ByteString using CTR mode.
ctrCombine :: BlockCipher c =>
    c -> B.ByteString -> B.ByteString -> B.ByteString
ctrCombine cipher iv input
    | keyBlocks > 256 ^ ctrSize = error "Counter overflow"
    | otherwise = xorBytes (B.take (B.length input) keystream) input
    where
        (d, r) = B.length input `divMod` blockSize cipher
        keyBlocks = fromIntegral $ d + if r == 0 then 0 else 1
        ctrSize = fromIntegral $ blockSize cipher `div` 2

        toCtrVal = constantPad ctrSize 0 . B.reverse . integerToBytes
        ctrVals = map toCtrVal [0..keyBlocks-1]
        keystream = B.concat $ map (ecbEncrypt cipher . B.append iv) ctrVals

