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


-- | A compression function for a Merkle-Damgård hash function,
-- accepting a state and a block of input and returning a new state.
type HashFunc = B.ByteString -> B.ByteString -> B.ByteString

-- | A collision in a compression function, including the number of
-- invocations required to find the collision, the initial state, and
-- the two colliding input blocks.
data Collision = Collision {
    getAttempts :: Integer,
    getBlockA :: B.ByteString,
    getBlockB :: B.ByteString,
    getState :: B.ByteString
} deriving Show


-- | The block size in bytes for hash functions created by this module.
blockSize :: Integer
blockSize = fromIntegral $ CT.blockSize (undefined :: AES128)


-- | Creates a compression function for a weak Merkle-Damgård hash
-- function (weak in the sense that the state size can be very small).
-- The compression function is implemented by using the current state
-- as an AES128 key, encrypting the input block, and using the result
-- as the new state.
--
-- This function accepts a state size in bytes, as well as an offset
-- used when extracting states from the AES128 output block. For
-- example, @makeWeakHash 2 0@ and @makeWeakHash 2 2@ will each return
-- compression functions with a two-byte state size, but the first will
-- be based on the first two AES output bytes, while the second will be
-- based on the third and fourth output bytes.
--
-- Returns a (state size, compression function) tuple.
makeWeakHash :: Integer -> Integer -> (Integer, HashFunc)
makeWeakHash reqSize offset = (size, f)
    where
        dummyCipher = undefined :: AES128
        CT.KeySizeFixed keySize' = CT.cipherKeySize dummyCipher
        keySize = fromIntegral keySize'
        blockSize' = fromIntegral blockSize
        size = min keySize reqSize
        size' = fromIntegral size
        offset' = fromIntegral offset
        f state bytes
            | B.length state /= size' = error "Bad state size"
            | B.length bytes `mod` blockSize' /= 0 = error "Bad input size"
            | B.null bytes = state
            | otherwise = f state' bytesTail
            where
                key = constantPadLeft keySize 0 state
                cipher :: AES128
                CryptoPassed cipher = CT.cipherInit key

                bytesHead = B.take blockSize' bytes
                bytesTail = B.drop blockSize' bytes

                cipherOutput = CT.ecbEncrypt cipher bytesHead
                state' = B.take size' $ B.drop offset' cipherOutput


-- | Takes the big-endian representation of an integer and pads it to
-- the block size.
toBlock :: Integer -> B.ByteString
toBlock = constantPadLeft blockSize 0 . integerToBytes

-- | The number of possible blocks, minus 1.
maxBlock :: Integer
maxBlock = 2 ^ (blockSize * 8) - 1

-- | A list of all possible blocks of the appropriate length.
allBlocks :: [B.ByteString]
allBlocks = map toBlock [0..maxBlock]


-- | Finds two blocks that collide starting from a common state.
findCollision1 :: HashFunc -> B.ByteString -> Collision
findCollision1 f state = _find1 f 0 state Map.empty allBlocks


-- | Finds two blocks that collide starting from two different states.
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
