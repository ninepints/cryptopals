-- Based on pseudocode from https://en.wikipedia.org/wiki/Mersenne_Twister
-- (most of these single-letter variable names are on them)
module SpecImplementations.Mersenne (
    seedGen,
    genFromState,
    stateLength,
    next,
    temper,
    untemper,
    sketchyCtr,
    MersenneGen
) where

import Control.Monad.State (evalState, state)
import Data.Array.IArray ((!), array, Array)
import Data.Bits ((.&.), (.|.), bit, complement, finiteBitSize, shiftL, shiftR,
                  testBit, xor, FiniteBits)
import Data.Word (Word32)
import Text.Printf (printf)

import qualified Data.ByteString.Common as B
import Util (xorBytes)


w :: Word32
w = 32

n :: Word32
n = 624

m :: Word32
m = 397

r :: Word32
r = 31

a :: Word32
a = 0x9908b0df

u :: Word32
u = 11

d :: Word32
d = 0xffffffff

s :: Word32
s = 7

b :: Word32
b = 0x9d2c5680

t :: Word32
t = 15

c :: Word32
c = 0xefc60000

l :: Word32
l = 18

f :: Word32
f = 1812433253

-- ((.&.) $ shiftL 1 (fromIntegral w) - 1) per Wikipedia's pseudocode,
-- but taking the low 32 bits of a 32-bit value is kinda silly
lowWBits :: Word32 -> Word32
lowWBits = id

lowerMask :: Word32
lowerMask = shiftL 1 (fromIntegral r) - 1

upperMask :: Word32
upperMask = lowWBits $ complement lowerMask


data MersenneGen = MersenneGen {
    getMT :: Array Word32 Word32,
    getIndex :: Word32
}


seedGen :: Word32 -> MersenneGen
seedGen seed = MersenneGen mt n
    where
        mt = array (0, n-1) ((0, seed) : map wordAt [1..n-1])
        wordAt i = (i, lowWBits $ f * (prev `xor` prevShifted) + i)
            where
                prev = mt!(i-1)
                prevShifted = shiftR prev $ fromIntegral $ w - 2


genFromState :: [Word32] -> MersenneGen
genFromState state | length state == fromIntegral n = MersenneGen mt n
                   | otherwise = error msg
    where
        mt = array (0, n-1) $ zip [0..n-1] state
        msg = printf "State has %u elements (expected %u)" (length state) n


stateLength :: Integer
stateLength = fromIntegral n


next :: MersenneGen -> (Word32, MersenneGen)
next gen@(MersenneGen mt i) | i == n = next $ twist gen
                            | otherwise = (temper (mt!i), MersenneGen mt (i+1))


temper :: Word32 -> Word32
temper = lowWBits . (\x -> x `xor` shiftR x (fromIntegral l)) .
    (\x -> x `xor` shiftL x (fromIntegral t) .&. c) .
    (\x -> x `xor` shiftL x (fromIntegral s) .&. b) .
    (\x -> x `xor` shiftR x (fromIntegral u) .&. d)


untemper :: Word32 -> Word32
untemper = lowWBits .
    reverseShift (fromIntegral u) False d .
    reverseShift (fromIntegral s) True b .
    reverseShift (fromIntegral t) True c .
    reverseShift (fromIntegral l) False (-1)
    where
        reverseShift :: Integer -> Bool -> Word32 -> Word32 -> Word32
        reverseShift n wasLeft mask bits = foldl1 (.|.) origChunks
            where
                chunk = chunksOf n (not wasLeft)
                maskChunks = 0 : tail (chunk mask)
                bitChunks = chunk bits
                origChunks = reverseShift' n wasLeft maskChunks bitChunks

        reverseShift' :: FiniteBits a => Integer -> Bool -> [a] -> [a] -> [a]
        reverseShift' _ _ [] [] = []
        reverseShift' _ _ (m:[]) (x:[]) = [m `xor` x]
        reverseShift' n wasLeft (m:m':ms) (x:xs) = thisChunk : nextChunks
            where
                thisChunk = m `xor` x
                shift = if wasLeft then shiftL else shiftR
                nextMask = m' .&. shift thisChunk (fromIntegral n)
                nextChunks = reverseShift' n wasLeft (nextMask : ms) xs


-- | Similar to Chunkable.chunksOf, but embeds results within an all-
-- zero series of bytes. Also supports reversing the input.
--
-- >>> chunksOf 7 True 0x55
-- [0x54, 0x01]
-- >>> chunksOf 3 False 0x55
-- [0x01, 0x14, 0x40]
chunksOf :: FiniteBits a => Integer -> Bool -> a -> [a]
chunksOf n fromLeft bits = map getChunk [0..numChunks-1]
    where
        m = fromIntegral $ finiteBitSize bits
        (d, r) = m `divMod` n
        numChunks = d + if r == 0 then 0 else 1
        bitFromEnd = bit . fromIntegral . if fromLeft then (-) (m - 1) else id
        getChunk i = bits .&. foldl1 (.|.) maskBits
            where maskBits = map bitFromEnd [i * n .. (i+1) * n - 1]


twist :: MersenneGen -> MersenneGen
twist (MersenneGen mt _) = MersenneGen mt' 0
    where
        mt' = array (0, n-1) $ map wordAt [0..n-1]
        wordAt i = (i, getWrapping (i+m) `xor` xa)
            where
                getWrapping j = (if j `mod` n < i then mt' else mt)!(j `mod` n)
                xUpper = getWrapping i .&. upperMask
                xLower = getWrapping (i+1) .&. lowerMask
                x = xUpper + xLower
                xa = if testBit x 0 then shiftR x 1 `xor` a else shiftR x 1


sketchyCtr :: B.ByteString a => Word32 -> a -> a
sketchyCtr seed input = xorBytes (B.take (B.length input) keystream) input
    where
        (d, r) = B.length input `divMod` 4
        numOutputs = fromIntegral $ d + if r == 0 then 0 else 1
        gen = seedGen seed
        outputs = evalState (sequence $ replicate numOutputs $ state next) gen
        keystream = B.pack $ outputs >>= (\output -> map fromIntegral [
                output .&. 0x000000ff,
                (output .&. 0x0000ff00) `shiftR` 8,
                (output .&. 0x00ff0000) `shiftR` 16,
                (output .&. 0xff000000) `shiftR` 24
            ])
