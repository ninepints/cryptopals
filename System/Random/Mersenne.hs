-- Based on pseudocode from https://en.wikipedia.org/wiki/Mersenne_Twister
-- (all these single-letter variable names are on them)
module System.Random.Mersenne (seedGen, next, MersenneGen) where

import Data.Array.IArray ((!), array, Array)
import Data.Bits ((.&.), complement, shiftL, shiftR, testBit, xor)
import Data.Word (Word32)


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


next :: MersenneGen -> (Word32, MersenneGen)
next gen@(MersenneGen mt i) | i == n = next $ twist gen
                            | otherwise = (temper (mt!i), MersenneGen mt (i+1))


temper :: Word32 -> Word32
temper = lowWBits . (\x -> x `xor` shiftR x (fromIntegral l)) .
    (\x -> x `xor` shiftL x (fromIntegral t) .&. c) .
    (\x -> x `xor` shiftL x (fromIntegral s) .&. b) .
    (\x -> x `xor` shiftR x (fromIntegral u) .&. d)


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
