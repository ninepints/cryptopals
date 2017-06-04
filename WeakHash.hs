module WeakHash (
    blockSize,
    makeWeakHash,
    allBlocks,
    findCollision1,
    findCollision2,
    HashFunc,
    Collision(..)
) where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Crypto.Cipher.AES (AES128)
import qualified Crypto.Cipher.Types as CT
import Crypto.Error (CryptoFailable(..))

import ByteFormat (integerToBytes)
import Padding (constantPadLeft)


type HashFunc = B.ByteString -> B.ByteString -> B.ByteString

data Collision = Collision {
    getAttempts :: Integer,
    getBlockA :: B.ByteString,
    getBlockB :: B.ByteString,
    getState :: B.ByteString
} deriving Show


blockSize :: Integer
blockSize = fromIntegral $ CT.blockSize (undefined :: AES128)


makeWeakHash :: Integer -> Integer -> (Integer, HashFunc)
makeWeakHash size offset = (size, f)
    where
        size' = fromIntegral size
        offset' = fromIntegral offset
        f state bytes
            | B.length state /= size' = error "Bad state size"
            | B.length bytes `mod` blockSize' /= 0 = error "Bad input size"
            | B.null bytes = state
            | otherwise = f state' bytesTail
            where
                cipher :: AES128
                CT.KeySizeFixed keySize = CT.cipherKeySize cipher
                keySize' = fromIntegral keySize
                blockSize' = fromIntegral blockSize
                key = constantPadLeft keySize' 0 state
                CryptoPassed cipher = CT.cipherInit key

                bytesHead = B.take blockSize' bytes
                bytesTail = B.drop blockSize' bytes

                cipherOutput = CT.ecbEncrypt cipher bytesHead
                state' = B.take size' $ B.drop offset' cipherOutput


toBlock :: Integer -> B.ByteString
toBlock = constantPadLeft blockSize 0 . integerToBytes

maxBlock :: Integer
maxBlock = 2 ^ (blockSize * 8) - 1

allBlocks :: [B.ByteString]
allBlocks = map toBlock [0..maxBlock]


findCollision1 :: HashFunc -> B.ByteString -> Collision
findCollision1 f state = _find1 f 0 state Map.empty allBlocks


findCollision2 :: HashFunc -> B.ByteString -> B.ByteString -> Collision
findCollision2 f stateA stateB = if stateA == stateB
    then _find1 f 0 stateA Map.empty allBlocks
    else _find2 f 0 False stateA Map.empty allBlocks stateB Map.empty allBlocks


_find1 :: HashFunc -> Integer ->
    B.ByteString -> Map.Map B.ByteString B.ByteString -> [B.ByteString] ->
    Collision
_find1 f attempts state outputMap inputs = collision
    where
        (inputA : inputs') = inputs
        output = f state inputA
        outputMap' = Map.insert output inputA outputMap
        attempts' = attempts + 1

        collision = case Map.lookup output outputMap of
            Just inputB -> Collision attempts' inputA inputB output
            Nothing -> _find1 f attempts' state outputMap' inputs'


_find2 :: HashFunc -> Integer -> Bool ->
    B.ByteString -> Map.Map B.ByteString B.ByteString -> [B.ByteString] ->
    B.ByteString -> Map.Map B.ByteString B.ByteString -> [B.ByteString] ->
    Collision
_find2 f attempts swapped stateA mapA inputsA stateB mapB inputsB = c
    where
        (inputA : inputsA') = inputsA
        outputA = f stateA inputA
        mapA' = Map.insert outputA inputA mapA
        attempts' = attempts + 1

        c = case (Map.lookup outputA mapB, swapped) of
            (Just inputB, False) -> Collision attempts' inputA inputB outputA
            (Just inputB, True) -> Collision attempts' inputB inputA outputA
            (Nothing, _) -> _find2 f attempts' (not swapped)
                stateB mapB inputsB stateA mapA' inputsA'
