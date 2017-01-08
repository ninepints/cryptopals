module BlockCipher (cbcEncrypt, cbcDecrypt) where

import qualified Data.ByteString as B

import Crypto.Cipher.Types (blockSize, ecbEncrypt, ecbDecrypt, BlockCipher)

import qualified Data.Chunkable as C
import Util (xorBytes)


blockAdapt :: BlockCipher c =>
    (c -> B.ByteString -> [B.ByteString] -> [B.ByteString]) ->
    c -> B.ByteString -> B.ByteString -> B.ByteString
blockAdapt blockFunc cipher iv = join . blockFunc cipher iv . split
    where
        split = C.chunksOf $ fromIntegral $ blockSize cipher
        join = C.concat


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
