module HMAC (hmac) where

import qualified Data.ByteString as B

import Crypto.Hash (HashAlgorithm)
import Crypto.Hash.IO (hashBlockSize)

import Padding (constantPad)
import Util (hash, xorBytes)


hmac :: HashAlgorithm a => a -> B.ByteString -> B.ByteString -> B.ByteString
hmac alg key = hash alg . B.append outerMask . hash alg . B.append innerMask
    where
        blockSize = hashBlockSize alg
        key' = if B.length key > blockSize then hash alg key else key
        key'' = if B.length key' < blockSize
            then constantPad (fromIntegral blockSize) 0 key' else key'

        outerMask = xorBytes (B.replicate blockSize 0x5c) key''
        innerMask = xorBytes (B.replicate blockSize 0x36) key''
