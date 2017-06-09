import Control.Monad (when)
import Control.Monad.State (get, put, execState, State)
import Data.Bits ((.&.), (.|.), xor, complement, bit, testBit,
                  shiftL, shiftR, rotateL, rotateR)
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8, Word32)
import System.Environment (getArgs)

import Crypto.Hash.Algorithms (MD4(..))

import ByteFormat (bytesToHex)
import Data.Chunkable (chunksOf)
import Util (randomBytesIO, xorBytes, hash)


data MD4State = MD4State Word32 Word32 Word32 Word32

initState :: MD4State
initState = MD4State 0x67452301 0xefcdab89 0x98badcfe 0x10325476


data MD4Ingestion = MD4Ingestion {
    getM0 :: Word32,
    getM1 :: Word32,
    getM2 :: Word32,
    getM3 :: Word32,
    getM4 :: Word32,
    getM5 :: Word32,
    getM6 :: Word32,
    getM7 :: Word32,
    getM8 :: Word32,
    getM9 :: Word32,
    getM10 :: Word32,
    getM11 :: Word32,
    getM12 :: Word32,
    getM13 :: Word32,
    getM14 :: Word32,
    getM15 :: Word32,

    getA0 :: Word32,
    getB0 :: Word32,
    getC0 :: Word32,
    getD0 :: Word32,

    getA1 :: Word32,
    getB1 :: Word32,
    getC1 :: Word32,
    getD1 :: Word32,

    getA2 :: Word32,
    getB2 :: Word32,
    getC2 :: Word32,
    getD2 :: Word32,

    getA3 :: Word32,
    getB3 :: Word32,
    getC3 :: Word32,
    getD3 :: Word32,

    getA4 :: Word32,
    getB4 :: Word32,
    getC4 :: Word32,
    getD4 :: Word32,

    getA5 :: Word32,
    getB5 :: Word32,
    getC5 :: Word32,
    getD5 :: Word32,

    getA6 :: Word32,
    getC6 :: Word32,
    getD6 :: Word32
}


main :: IO ()
main = do
    args <- getArgs
    findCollision ("--debug" `elem` args) 1

findCollision :: Bool -> Integer -> IO ()
findCollision debug i = do
    origMessage <- randomBytesIO 64
    let tweakedMessage = tweakBlock initState origMessage
        tweakedHash = hash MD4 tweakedMessage
        collidingMessage = applyDifferential tweakedMessage
        collidingHash = hash MD4 collidingMessage

    if tweakedHash /= collidingHash
        then do
            when debug $ checkConditions tweakedMessage
            when (i `mod` 1000 == 0) $ putStrLn $ "Finished iter " ++ show i
            findCollision debug (i+1)
        else do
            putStrLn $ "Found collision after " ++ show i ++ " iterations"
            let showHex = BC.unpack . bytesToHex
            putStrLn $ "Original message is:  " ++ showHex origMessage
            putStrLn $ "Tweaked message is:   " ++ showHex tweakedMessage
            putStrLn $ "...hash is " ++ showHex tweakedHash
            putStrLn $ "Colliding message is: " ++ showHex collidingMessage
            putStrLn $ "...hash is " ++ showHex collidingHash


tweakBlock :: MD4State -> B.ByteString -> B.ByteString
tweakBlock (MD4State a0 b0 c0 d0) = fromState . execState tweak . toState
    where
        toState bytes = MD4Ingestion
            m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15
            a0 b0 c0 d0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            where
                [m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15] =
                    (chunksOf 4 $ B.unpack bytes) >>= ((:[]) . concatBytes)

        fromState (MD4Ingestion
            m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15
            _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
                B.pack $
                    [m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15] >>=
                        splitBytes

        tweak = do
            fixA1 >> recalcM0
            fixD1 >> recalcM1
            fixC1 >> recalcM2
            fixB1 >> recalcM3
            fixA2 >> recalcM4
            fixD2 >> recalcM5
            fixC2 >> recalcM6
            fixB2 >> recalcM7
            fixA3 >> recalcM8
            fixD3 >> recalcM9
            fixC3 >> recalcM10
            fixB3 >> recalcM11
            fixA4 >> recalcM12
            fixD4 >> recalcM13
            fixC4 >> recalcM14
            fixB4 >> recalcM15

            fixA5 >> fixA1
            recalcM0 >> recalcM1 >> recalcM2 >> recalcM3 >> recalcM4
            fixD5 >> fixA2
            recalcM4 >> recalcM5 >> recalcM6 >> recalcM7 >> recalcM8
            fixC5 >> fixA3
            recalcM8 >> recalcM9 >> recalcM10 >> recalcM11 >> recalcM12
            fixB5 >> fixA4
            recalcM12 >> recalcM13 >> recalcM14 >> recalcM15
            fixA6 >> fixD1
            recalcM1 >> recalcM2 >> recalcM3 >> recalcM4 >> recalcM5
            fixD6 >> fixD2
            recalcM5 >> recalcM6 >> recalcM7 >> recalcM8 >> recalcM9
            fixC6 >> fixD3
            recalcM9 >> recalcM10 >> recalcM11 >> recalcM12 >> recalcM13


f :: Word32 -> Word32 -> Word32 -> Word32
f x y z = (x .&. y) .|. ((complement x) .&. z)

g :: Word32 -> Word32 -> Word32 -> Word32
g x y z = (x .&. y) .|.  (x .&. z) .|. (y .&. z)

k :: Word32
k = 0x5a827999

setBit :: Int -> Word32 -> Word32
setBit = flip Bits.setBit

clearBit :: Int -> Word32 -> Word32
clearBit = flip Bits.clearBit

copyBit :: Word32 -> Int -> Word32 -> Word32
copyBit from i to = to `xor` ((from `xor` to) .&. bit i)

copyInv :: Word32 -> Int -> Word32 -> Word32
copyInv from i to = to `xor` (complement (from `xor` to) .&. bit i)

liftA5 :: Applicative app => (a -> b -> c -> d -> e -> f) ->
    app a -> app b -> app c -> app d -> app e -> app f
liftA5 app a b c d e = fmap app a <*> b <*> c <*> d <*> e


-- a1,7 = b0,7
fixA1 :: State MD4Ingestion ()
fixA1 = do
    s <- get
    let (a0, b0, c0, d0, m0) = liftA5 (,,,,) getA0 getB0 getC0 getD0 getM0 s
        a1 = (a0 + f b0 c0 d0 + m0) `rotateL` 3
        a1' = copyBit b0 6 a1
    put $ s { getA1 = a1' }

recalcM0 :: State MD4Ingestion ()
recalcM0 = do
    s <- get
    let (a0, b0, c0, d0, a1) = liftA5 (,,,,) getA0 getB0 getC0 getD0 getA1 s
        m0 = a1 `rotateR` 3 - a0 - f b0 c0 d0
    put $ s { getM0 = m0 }


-- d1,7 = 0
-- d1,8 = a1,8
-- d1,11 = a1,11
fixD1 :: State MD4Ingestion ()
fixD1 = do
    s <- get
    let (d0, a1, b0, c0, m1) = liftA5 (,,,,) getD0 getA1 getB0 getC0 getM1 s
        d1 = (d0 + f a1 b0 c0 + m1) `rotateL` 7
        d1' = copyBit a1 10 $
              copyBit a1 7 $
              clearBit 6 d1
    put $ s { getD1 = d1' }

recalcM1 :: State MD4Ingestion ()
recalcM1 = do
    s <- get
    let (d0, a1, b0, c0, d1) = liftA5 (,,,,) getD0 getA1 getB0 getC0 getD1 s
        m1 = d1 `rotateR` 7 - d0 - f a1 b0 c0
    put $ s { getM1 = m1 }


-- c1,7 = 1
-- c1,8 = 1
-- c1,11 = 0
-- c1,26 = d1,26
fixC1 :: State MD4Ingestion ()
fixC1 = do
    s <- get
    let (c0, d1, a1, b0, m2) = liftA5 (,,,,) getC0 getD1 getA1 getB0 getM2 s
        c1 = (c0 + f d1 a1 b0 + m2) `rotateL` 11
        c1' = copyBit d1 25 $
              clearBit 10 $
              setBit 7 $
              setBit 6 c1
    put $ s { getC1 = c1' }

recalcM2 :: State MD4Ingestion ()
recalcM2 = do
    s <- get
    let (c0, d1, a1, b0, c1) = liftA5 (,,,,) getC0 getD1 getA1 getB0 getC1 s
        m2 = c1 `rotateR` 11 - c0 - f d1 a1 b0
    put $ s { getM2 = m2 }

-- b1,7 = 1
-- b1,8 = 0
-- b1,11 = 0
-- b1,26 = 0
fixB1 :: State MD4Ingestion ()
fixB1 = do
    s <- get
    let (b0, c1, d1, a1, m3) = liftA5 (,,,,) getB0 getC1 getD1 getA1 getM3 s
        b1 = (b0 + f c1 d1 a1 + m3) `rotateL` 19
        b1' = clearBit 25 $
              clearBit 10 $
              clearBit 7 $
              setBit 6 b1
    put $ s { getB1 = b1' }

recalcM3 :: State MD4Ingestion ()
recalcM3 = do
    s <- get
    let (b0, c1, d1, a1, b1) = liftA5 (,,,,) getB0 getC1 getD1 getA1 getB1 s
        m3 = b1 `rotateR` 19 - b0 - f c1 d1 a1
    put $ s { getM3 = m3 }


-- a2,8 = 1
-- a2,11 = 1
-- a2,14 = b1,14
-- a2,26 = 0
fixA2 :: State MD4Ingestion ()
fixA2 = do
    s <- get
    let (a1, b1, c1, d1, m4) = liftA5 (,,,,) getA1 getB1 getC1 getD1 getM4 s
        a2 = (a1 + f b1 c1 d1 + m4) `rotateL` 3
        a2' = clearBit 25 $
              copyBit b1 13 $
              setBit 10 $
              setBit 7 a2
    put $ s { getA2 = a2' }

recalcM4 :: State MD4Ingestion ()
recalcM4 = do
    s <- get
    let (a1, b1, c1, d1, a2) = liftA5 (,,,,) getA1 getB1 getC1 getD1 getA2 s
        m4 = a2 `rotateR` 3 - a1 - f b1 c1 d1
    put $ s { getM4 = m4 }


-- d2,14 = 0
-- d2,19 = a2,19
-- d2,20 = a2,20
-- d2,21 = a2,21
-- d2,22 = a2,22
-- d2,26 = 1
fixD2 :: State MD4Ingestion ()
fixD2 = do
    s <- get
    let (d1, a2, b1, c1, m5) = liftA5 (,,,,) getD1 getA2 getB1 getC1 getM5 s
        d2 = (d1 + f a2 b1 c1 + m5) `rotateL` 7
        d2' = setBit 25 $
              copyBit a2 21 $
              copyBit a2 20 $
              copyBit a2 19 $
              copyBit a2 18 $
              clearBit 13 d2
    put $ s { getD2 = d2' }

recalcM5 :: State MD4Ingestion ()
recalcM5 = do
    s <- get
    let (d1, a2, b1, c1, d2) = liftA5 (,,,,) getD1 getA2 getB1 getC1 getD2 s
        m5 = d2 `rotateR` 7 - d1 - f a2 b1 c1
    put $ s { getM5 = m5 }


-- c2,13 = d2,13
-- c2,14 = 0
-- c2,15 = d2,15
-- c2,19 = 0
-- c2,20 = 0
-- c2,21 = 1
-- c2,22 = 0
fixC2 :: State MD4Ingestion ()
fixC2 = do
    s <- get
    let (c1, d2, a2, b1, m6) = liftA5 (,,,,) getC1 getD2 getA2 getB1 getM6 s
        c2 = (c1 + f d2 a2 b1 + m6) `rotateL` 11
        c2' = clearBit 21 $
              setBit 20 $
              clearBit 19 $
              clearBit 18 $
              copyBit d2 14 $
              clearBit 13 $
              copyBit d2 12 c2
    put $ s { getC2 = c2' }

recalcM6 :: State MD4Ingestion ()
recalcM6 = do
    s <- get
    let (c1, d2, a2, b1, c2) = liftA5 (,,,,) getC1 getD2 getA2 getB1 getC2 s
        m6 = c2 `rotateR` 11 - c1 - f d2 a2 b1
    put $ s { getM6 = m6 }


-- b2,13 = 1
-- b2,14 = 1
-- b2,15 = 0
-- b2,17 = c2,17
-- b2,19 = 0
-- b2,20 = 0
-- b2,21 = 0
-- b2,22 = 0
fixB2 :: State MD4Ingestion ()
fixB2 = do
    s <- get
    let (b1, c2, d2, a2, m7) = liftA5 (,,,,) getB1 getC2 getD2 getA2 getM7 s
        b2 = (b1 + f c2 d2 a2 + m7) `rotateL` 19
        b2' = clearBit 21 $
              clearBit 20 $
              clearBit 19 $
              clearBit 18 $
              copyBit c2 16 $
              clearBit 14 $
              setBit 13 $
              setBit 12 b2
    put $ s { getB2 = b2' }

recalcM7 :: State MD4Ingestion ()
recalcM7 = do
    s <- get
    let (b1, c2, d2, a2, b2) = liftA5 (,,,,) getB1 getC2 getD2 getA2 getB2 s
        m7 = b2 `rotateR` 19 - b1 - f c2 d2 a2
    put $ s { getM7 = m7 }


-- a3,13 = 1
-- a3,14 = 1
-- a3,15 = 1
-- a3,17 = 0
-- a3,19 = 0
-- a3,20 = 0
-- a3,21 = 0
-- a3,22 = 1
-- a3,23 = b2,23
-- a3,26 = b2,26
fixA3 :: State MD4Ingestion ()
fixA3 = do
    s <- get
    let (a2, b2, c2, d2, m8) = liftA5 (,,,,) getA2 getB2 getC2 getD2 getM8 s
        a3 = (a2 + f b2 c2 d2 + m8) `rotateL` 3
        a3' = copyBit b2 25 $
              copyBit b2 22 $
              setBit 21 $
              clearBit 20 $
              clearBit 19 $
              clearBit 18 $
              clearBit 16 $
              setBit 14 $
              setBit 13 $
              setBit 12 a3
    put $ s { getA3 = a3' }

recalcM8 :: State MD4Ingestion ()
recalcM8 = do
    s <- get
    let (a2, b2, c2, d2, a3) = liftA5 (,,,,) getA2 getB2 getC2 getD2 getA3 s
        m8 = a3 `rotateR` 3 - a2 - f b2 c2 d2
    put $ s { getM8 = m8 }


-- d3,13 = 1
-- d3,14 = 1
-- d3,15 = 1
-- d3,17 = 0
-- d3,20 = 0
-- d3,21 = 1
-- d3,22 = 1
-- d3,23 = 0
-- d3,26 = 1
-- d3,30 = a3,30
fixD3 :: State MD4Ingestion ()
fixD3 = do
    s <- get
    let (d2, a3, b2, c2, m9) = liftA5 (,,,,) getD2 getA3 getB2 getC2 getM9 s
        d3 = (d2 + f a3 b2 c2 + m9) `rotateL` 7
        d3' = copyBit a3 29 $
              setBit 25 $
              clearBit 22 $
              setBit 21 $
              setBit 20 $
              clearBit 19 $
              clearBit 16 $
              setBit 14 $
              setBit 13 $
              setBit 12 d3
    put $ s { getD3 = d3' }

recalcM9 :: State MD4Ingestion ()
recalcM9 = do
    s <- get
    let (d2, a3, b2, c2, d3) = liftA5 (,,,,) getD2 getA3 getB2 getC2 getD3 s
        m9 = d3 `rotateR` 7 - d2 - f a3 b2 c2
    put $ s { getM9 = m9 }


-- c3,17 = 1
-- c3,20 = 0
-- c3,21 = 0
-- c3,22 = 0
-- c3,23 = 0
-- c3,26 = 0
-- c3,30 = 1
-- c3,32 = d3,32
fixC3 :: State MD4Ingestion ()
fixC3 = do
    s <- get
    let (c2, d3, a3, b2, m10) = liftA5 (,,,,) getC2 getD3 getA3 getB2 getM10 s
        c3 = (c2 + f d3 a3 b2 + m10) `rotateL` 11
        c3' = copyBit d3 31 $
              setBit 29 $
              clearBit 25 $
              clearBit 22 $
              clearBit 21 $
              clearBit 20 $
              clearBit 19 $
              setBit 16 c3
    put $ s { getC3 = c3' }

recalcM10 :: State MD4Ingestion ()
recalcM10 = do
    s <- get
    let (c2, d3, a3, b2, c3) = liftA5 (,,,,) getC2 getD3 getA3 getB2 getC3 s
        m10 = c3 `rotateR` 11 - c2 - f d3 a3 b2
    put $ s { getM10 = m10 }


-- b3,20 = 0
-- b3,21 = 1
-- b3,22 = 1
-- b3,23 = c3,23
-- b3,26 = 1
-- b3,30 = 0
-- b3,32 = 0
fixB3 :: State MD4Ingestion ()
fixB3 = do
    s <- get
    let (b2, c3, d3, a3, m11) = liftA5 (,,,,) getB2 getC3 getD3 getA3 getM11 s
        b3 = (b2 + f c3 d3 a3 + m11) `rotateL` 19
        b3' = clearBit 31 $
              clearBit 29 $
              setBit 25 $
              copyBit c3 22 $
              setBit 21 $
              setBit 20 $
              clearBit 19 b3
    put $ s { getB3 = b3' }

recalcM11 :: State MD4Ingestion ()
recalcM11 = do
    s <- get
    let (b2, c3, d3, a3, b3) = liftA5 (,,,,) getB2 getC3 getD3 getA3 getB3 s
        m11 = b3 `rotateR` 19 - b2 - f c3 d3 a3
    put $ s { getM11 = m11 }


-- a4,23 = 0
-- a4,26 = 0
-- a4,27 = b3,27
-- a4,29 = b3,29
-- a4,30 = 1
-- a4,32 = 0
fixA4 :: State MD4Ingestion ()
fixA4 = do
    s <- get
    let (a3, b3, c3, d3, m12) = liftA5 (,,,,) getA3 getB3 getC3 getD3 getM12 s
        a4 = (a3 + f b3 c3 d3 + m12) `rotateL` 3
        a4' = clearBit 31 $
              setBit 29 $
              copyBit b3 28 $
              copyBit b3 26 $
              clearBit 25 $
              clearBit 22 a4
    put $ s { getA4 = a4' }

recalcM12 :: State MD4Ingestion ()
recalcM12 = do
    s <- get
    let (a3, b3, c3, d3, a4) = liftA5 (,,,,) getA3 getB3 getC3 getD3 getA4 s
        m12 = a4 `rotateR` 3 - a3 - f b3 c3 d3
    put $ s { getM12 = m12 }


-- d4,23 = 0
-- d4,26 = 0
-- d4,27 = 1
-- d4,29 = 1
-- d4,30 = 0
-- d4,32 = 1
fixD4 :: State MD4Ingestion ()
fixD4 = do
    s <- get
    let (d3, a4, b3, c3, m13) = liftA5 (,,,,) getD3 getA4 getB3 getC3 getM13 s
        d4 = (d3 + f a4 b3 c3 + m13) `rotateL` 7
        d4' = setBit 31 $
              clearBit 29 $
              setBit 28 $
              setBit 26 $
              clearBit 25 $
              clearBit 22 d4
    put $ s { getD4 = d4' }

recalcM13 :: State MD4Ingestion ()
recalcM13 = do
    s <- get
    let (d3, a4, b3, c3, d4) = liftA5 (,,,,) getD3 getA4 getB3 getC3 getD4 s
        m13 = d4 `rotateR` 7 - d3 - f a4 b3 c3
    put $ s { getM13 = m13 }


-- c4,19 = d4,19
-- c4,23 = 1
-- c4,26 = 1
-- c4,27 = 0
-- c4,29 = 0
-- c4,30 = 0
fixC4 :: State MD4Ingestion ()
fixC4 = do
    s <- get
    let (c3, d4, a4, b3, m14) = liftA5 (,,,,) getC3 getD4 getA4 getB3 getM14 s
        c4 = (c3 + f d4 a4 b3 + m14) `rotateL` 11
        c4' = clearBit 29 $
              clearBit 28 $
              clearBit 26 $
              setBit 25 $
              setBit 22 $
              copyBit d4 18 c4
    put $ s { getC4 = c4' }

recalcM14 :: State MD4Ingestion ()
recalcM14 = do
    s <- get
    let (c3, d4, a4, b3, c4) = liftA5 (,,,,) getC3 getD4 getA4 getB3 getC4 s
        m14 = c4 `rotateR` 11 - c3 - f d4 a4 b3
    put $ s { getM14 = m14 }


-- b4,19 = 0
-- b4,26 = c4,26 = 1
-- b4,27 = 1
-- b4,29 = 1
-- b4,30 = 0
fixB4 :: State MD4Ingestion ()
fixB4 = do
    s <- get
    let (b3, c4, d4, a4, m15) = liftA5 (,,,,) getB3 getC4 getD4 getA4 getM15 s
        b4 = (b3 + f c4 d4 a4 + m15) `rotateL` 19
        b4' = clearBit 29 $
              setBit 28 $
              setBit 26 $
              setBit 25 $
              clearBit 18 b4
    put $ s { getB4 = b4' }

recalcM15 :: State MD4Ingestion ()
recalcM15 = do
    s <- get
    let (b3, c4, d4, a4, b4) = liftA5 (,,,,) getB3 getC4 getD4 getA4 getB4 s
        m15 = b4 `rotateR` 19 - b3 - f c4 d4 a4
    put $ s { getM15 = m15 }


-- a5,19 = c4,19
-- a5,26 = 1
-- a5,27 = 0
-- a5,29 = 1
-- a5,32 = 1
fixA5 :: State MD4Ingestion ()
fixA5 = do
    s <- get
    let (a4, b4, c4, d4, m0) = liftA5 (,,,,) getA4 getB4 getC4 getD4 getM0 s
        a5 = (a4 + g b4 c4 d4 + k + m0) `rotateL` 3
        a5' = setBit 31 $
              setBit 28 $
              clearBit 26 $
              setBit 25 $
              copyBit c4 18 a5
        m0' = a5' `rotateR` 3 - a4 - g b4 c4 d4 - k
    put $ s { getA5 = a5', getM0 = m0' }


-- d5,19 = a5,19
-- d5,26 = b4,26
-- d5,27 = b4,27
-- d5,29 = b4,29
-- d5,32 = b4,32
fixD5 :: State MD4Ingestion ()
fixD5 = do
    s <- get
    let (d4, a5, b4, c4, m4) = liftA5 (,,,,) getD4 getA5 getB4 getC4 getM4 s
        d5 = (d4 + g a5 b4 c4 + k + m4) `rotateL` 5
        d5' = copyBit b4 31 $
              copyBit b4 28 $
              copyBit b4 26 $
              copyBit b4 25 $
              copyBit a5 18 d5
        m4' = d5' `rotateR` 5 - d4 - g a5 b4 c4 - k
    put $ s { getD5 = d5', getM4 = m4' }


-- c5,26 = d5,26
-- c5,27 = d5,27
-- c5,29 = d5,29
-- c5,30 = d5,30
-- c5,32 = d5,32
fixC5 :: State MD4Ingestion ()
fixC5 = do
    s <- get
    let (c4, d5, a5, b4, m8) = liftA5 (,,,,) getC4 getD5 getA5 getB4 getM8 s
        c5 = (c4 + g d5 a5 b4 + k + m8) `rotateL` 9
        c5' = copyBit d5 31 $
              copyBit d5 29 $
              copyBit d5 28 $
              copyBit d5 26 $
              copyBit d5 25 c5
        m8' = c5' `rotateR` 9 - c4 - g d5 a5 b4 - k
    put $ s { getC5 = c5', getM8 = m8' }


-- b5,29 = c5,29
-- b5,30 = 1
-- b5,32 = 0
fixB5 :: State MD4Ingestion ()
fixB5 = do
    s <- get
    let (b4, c5, d5, a5, m12) = liftA5 (,,,,) getB4 getC5 getD5 getA5 getM12 s
        b5 = (b4 + g c5 d5 a5 + k + m12) `rotateL` 13
        b5' = clearBit 31 $
              setBit 29 $
              copyBit c5 28 b5
        m12' = b5' `rotateR` 13 - b4 - g c5 d5 a5 - k
    put $ s { getB5 = b5', getM12 = m12' }


-- a6,29 = 1
-- a6,32 = 1
fixA6 :: State MD4Ingestion ()
fixA6 = do
    s <- get
    let (a5, b5, c5, d5, m1) = liftA5 (,,,,) getA5 getB5 getC5 getD5 getM1 s
        a6 = (a5 + g b5 c5 d5 + k + m1) `rotateL` 3
        a6' = setBit 31 $
              setBit 28 a6
        m1' = a6' `rotateR` 3 - a5 - g b5 c5 d5 - k
    put $ s { getA6 = a6', getM1 = m1' }


-- d6,29 = b5,29
fixD6 :: State MD4Ingestion ()
fixD6 = do
    s <- get
    let (d5, a6, b5, c5, m5) = liftA5 (,,,,) getD5 getA6 getB5 getC5 getM5 s
        d6 = (d5 + g a6 b5 c5 + k + m5) `rotateL` 5
        d6' = copyBit b5 28 d6
        m5' = d6' `rotateR` 5 - d5 - g a6 b5 c5 - k
    put $ s { getD6 = d6', getM5 = m5' }


-- I assume when the paper says +1 it means flip the bit
-- c6,29 = d6,29
-- c6,30 = d6,30 + 1
-- c6,32 = d6,32 + 1
fixC6 :: State MD4Ingestion ()
fixC6 = do
    s <- get
    let (c5, d6, a6, b5, m9) = liftA5 (,,,,) getC5 getD6 getA6 getB5 getM9 s
        c6 = (c5 + g d6 a6 b5 + k + m9) `rotateL` 9
        c6' = copyInv d6 31 $
              copyInv d6 29 $
              copyBit d6 28 c6
        m9' = c6' `rotateR` 9 - c5 - g d6 a6 b5 - k
    put $ s { getC6 = c6', getM9 = m9' }


-- Skipping the last two (in the third comp. function round)
-- b9,32 = 1
-- a10,32 = 1


checkConditions :: B.ByteString -> IO ()
checkConditions bytes = do
    let (MD4State a0 b0 c0 d0) = initState
        [m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15] =
            (chunksOf 4 $ B.unpack bytes) >>= ((:[]) . concatBytes)

        a1 = (a0 + f b0 c0 d0 + m0) `rotateL` 3
        d1 = (d0 + f a1 b0 c0 + m1) `rotateL` 7
        c1 = (c0 + f d1 a1 b0 + m2) `rotateL` 11
        b1 = (b0 + f c1 d1 a1 + m3) `rotateL` 19
        a2 = (a1 + f b1 c1 d1 + m4) `rotateL` 3
        d2 = (d1 + f a2 b1 c1 + m5) `rotateL` 7
        c2 = (c1 + f d2 a2 b1 + m6) `rotateL` 11
        b2 = (b1 + f c2 d2 a2 + m7) `rotateL` 19
        a3 = (a2 + f b2 c2 d2 + m8) `rotateL` 3
        d3 = (d2 + f a3 b2 c2 + m9) `rotateL` 7
        c3 = (c2 + f d3 a3 b2 + m10) `rotateL` 11
        b3 = (b2 + f c3 d3 a3 + m11) `rotateL` 19
        a4 = (a3 + f b3 c3 d3 + m12) `rotateL` 3
        d4 = (d3 + f a4 b3 c3 + m13) `rotateL` 7
        c4 = (c3 + f d4 a4 b3 + m14) `rotateL` 11
        b4 = (b3 + f c4 d4 a4 + m15) `rotateL` 19

        a5 = (a4 + g b4 c4 d4 + k + m0) `rotateL` 3
        d5 = (d4 + g a5 b4 c4 + k + m4) `rotateL` 5
        c5 = (c4 + g d5 a5 b4 + k + m8) `rotateL` 9
        b5 = (b4 + g c5 d5 a5 + k + m12) `rotateL` 13
        a6 = (a5 + g b5 c5 d5 + k + m1) `rotateL` 3
        d6 = (d5 + g a6 b5 c5 + k + m5) `rotateL` 5
        c6 = (c5 + g d6 a6 b5 + k + m9) `rotateL` 9

        a1_7 = testBit a1 6 == testBit b0 6

        d1_7 = not $ testBit d1 6
        d1_8 = testBit d1 7 == testBit a1 7
        d1_11 = testBit d1 10 == testBit a1 10

        c1_7 = testBit c1 6
        c1_8 = testBit c1 7
        c1_11 = not $ testBit c1 10
        c1_26 = testBit c1 25 == testBit d1 25

        b1_7 = testBit b1 6
        b1_8 = not $ testBit b1 7
        b1_11 = not $ testBit b1 10
        b1_26 = not $ testBit b1 25

        a2_8 = testBit a2 7
        a2_11 = testBit a2 10
        a2_14 = testBit a2 13 == testBit b1 13
        a2_26 = not $ testBit a2 25

        d2_14 = not $ testBit d2 13
        d2_19 = testBit d2 18 == testBit a2 18
        d2_20 = testBit d2 19 == testBit a2 19
        d2_21 = testBit d2 20 == testBit a2 20
        d2_22 = testBit d2 21 == testBit a2 21
        d2_26 = testBit d2 25

        c2_13 = testBit c2 12 == testBit d2 12
        c2_14 = not $ testBit c2 13
        c2_15 = testBit c2 14 == testBit d2 14
        c2_19 = not $ testBit c2 18
        c2_20 = not $ testBit c2 19
        c2_21 = testBit c2 20
        c2_22 = not $ testBit c2 21

        b2_13 = testBit b2 12
        b2_14 = testBit b2 13
        b2_15 = not $ testBit b2 14
        b2_17 = testBit b2 16 == testBit c2 16
        b2_19 = not $ testBit b2 18
        b2_20 = not $ testBit b2 19
        b2_21 = not $ testBit b2 20
        b2_22 = not $ testBit b2 21

        a3_13 = testBit a3 12
        a3_14 = testBit a3 13
        a3_15 = testBit a3 14
        a3_17 = not $ testBit a3 16
        a3_19 = not $ testBit a3 18
        a3_20 = not $ testBit a3 19
        a3_21 = not $ testBit a3 20
        a3_22 = testBit a3 21
        a3_23 = testBit a3 22 == testBit b2 22
        a3_26 = testBit a3 25 == testBit b2 25

        d3_13 = testBit d3 12
        d3_14 = testBit d3 13
        d3_15 = testBit d3 14
        d3_17 = not $ testBit d3 16
        d3_20 = not $ testBit d3 19
        d3_21 = testBit d3 20
        d3_22 = testBit d3 21
        d3_23 = not $ testBit d3 22
        d3_26 = testBit d3 25
        d3_30 = testBit d3 29 == testBit a3 29

        c3_17 = testBit c3 16
        c3_20 = not $ testBit c3 19
        c3_21 = not $ testBit c3 20
        c3_22 = not $ testBit c3 21
        c3_23 = not $ testBit c3 22
        c3_26 = not $ testBit c3 25
        c3_30 = testBit c3 29
        c3_32 = testBit c3 31 == testBit d3 31

        b3_20 = not $ testBit b3 19
        b3_21 = testBit b3 20
        b3_22 = testBit b3 21
        b3_23 = testBit b3 22 == testBit c3 22
        b3_26 = testBit b3 25
        b3_30 = not $ testBit b3 29
        b3_32 = not $ testBit b3 31

        a4_23 = not $ testBit a4 22
        a4_26 = not $ testBit a4 25
        a4_27 = testBit a4 26 == testBit b3 26
        a4_29 = testBit a4 28 == testBit b3 28
        a4_30 = testBit a4 29
        a4_32 = not $ testBit a4 31

        d4_23 = not $ testBit d4 22
        d4_26 = not $ testBit d4 25
        d4_27 = testBit d4 26
        d4_29 = testBit d4 28
        d4_30 = not $ testBit d4 29
        d4_32 = testBit d4 31

        c4_19 = testBit c4 18 == testBit d4 18
        c4_23 = testBit c4 22
        c4_26 = testBit c4 25
        c4_27 = not $ testBit c4 26
        c4_29 = not $ testBit c4 28
        c4_30 = not $ testBit c4 29

        b4_19 = not $ testBit b4 18
        b4_26 = testBit b4 25
        b4_27 = testBit b4 26
        b4_29 = testBit b4 28
        b4_30 = not $ testBit b4 29

        a5_19 = testBit a5 18 == testBit c4 18
        a5_26 = testBit a5 25
        a5_27 = not $ testBit a5 26
        a5_29 = testBit a5 28
        a5_32 = testBit a5 31

        d5_19 = testBit d5 18 == testBit a5 18
        d5_26 = testBit d5 25 == testBit b4 25
        d5_27 = testBit d5 26 == testBit b4 26
        d5_29 = testBit d5 28 == testBit b4 28
        d5_32 = testBit d5 31 == testBit b4 31

        c5_26 = testBit c5 25 == testBit d5 25
        c5_27 = testBit c5 26 == testBit d5 26
        c5_29 = testBit c5 28 == testBit d5 28
        c5_30 = testBit c5 29 == testBit d5 29
        c5_32 = testBit c5 31 == testBit d5 31

        b5_29 = testBit b5 28 == testBit c5 28
        b5_30 = testBit b5 29
        b5_32 = not $ testBit b5 31

        a6_29 = testBit a6 28
        a6_32 = testBit a6 31

        d6_29 = testBit d6 28 == testBit b5 28

        c6_29 = testBit c6 28 == testBit d6 28
        c6_30 = testBit c6 29 /= testBit d6 29
        c6_32 = testBit c6 31 /= testBit d6 31

        conds = [
                (a1_7, "a1_7"),
                (d1_7, "d1_7"),
                (d1_8, "d1_8"),
                (d1_11, "d1_11"),
                (c1_7, "c1_7"),
                (c1_8, "c1_8"),
                (c1_11, "c1_11"),
                (c1_26, "c1_26"),
                (b1_7, "b1_7"),
                (b1_8, "b1_8"),
                (b1_11, "b1_11"),
                (b1_26, "b1_26"),
                (a2_8, "a2_8"),
                (a2_11, "a2_11"),
                (a2_14, "a2_14"),
                (a2_26, "a2_26"),
                (d2_14, "d2_14"),
                (d2_19, "d2_19"),
                (d2_20, "d2_20"),
                (d2_21, "d2_21"),
                (d2_22, "d2_22"),
                (d2_26, "d2_26"),
                (c2_13, "c2_13"),
                (c2_14, "c2_14"),
                (c2_15, "c2_15"),
                (c2_19, "c2_19"),
                (c2_20, "c2_20"),
                (c2_21, "c2_21"),
                (c2_22, "c2_22"),
                (b2_13, "b2_13"),
                (b2_14, "b2_14"),
                (b2_15, "b2_15"),
                (b2_17, "b2_17"),
                (b2_19, "b2_19"),
                (b2_20, "b2_20"),
                (b2_21, "b2_21"),
                (b2_22, "b2_22"),
                (a3_13, "a3_13"),
                (a3_14, "a3_14"),
                (a3_15, "a3_15"),
                (a3_17, "a3_17"),
                (a3_19, "a3_19"),
                (a3_20, "a3_20"),
                (a3_21, "a3_21"),
                (a3_22, "a3_22"),
                (a3_23, "a3_23"),
                (a3_26, "a3_26"),
                (d3_13, "d3_13"),
                (d3_14, "d3_14"),
                (d3_15, "d3_15"),
                (d3_17, "d3_17"),
                (d3_20, "d3_20"),
                (d3_21, "d3_21"),
                (d3_22, "d3_22"),
                (d3_23, "d3_23"),
                (d3_26, "d3_26"),
                (d3_30, "d3_30"),
                (c3_17, "c3_17"),
                (c3_20, "c3_20"),
                (c3_21, "c3_21"),
                (c3_22, "c3_22"),
                (c3_23, "c3_23"),
                (c3_26, "c3_26"),
                (c3_30, "c3_30"),
                (c3_32, "c3_32"),
                (b3_20, "b3_20"),
                (b3_21, "b3_21"),
                (b3_22, "b3_22"),
                (b3_23, "b3_23"),
                (b3_26, "b3_26"),
                (b3_30, "b3_30"),
                (b3_32, "b3_32"),
                (a4_23, "a4_23"),
                (a4_26, "a4_26"),
                (a4_27, "a4_27"),
                (a4_29, "a4_29"),
                (a4_30, "a4_30"),
                (a4_32, "a4_32"),
                (d4_23, "d4_23"),
                (d4_26, "d4_26"),
                (d4_27, "d4_27"),
                (d4_29, "d4_29"),
                (d4_30, "d4_30"),
                (d4_32, "d4_32"),
                (c4_19, "c4_19"),
                (c4_23, "c4_23"),
                (c4_26, "c4_26"),
                (c4_27, "c4_27"),
                (c4_29, "c4_29"),
                (c4_30, "c4_30"),
                (b4_19, "b4_19"),
                (b4_26, "b4_26"),
                (b4_27, "b4_27"),
                (b4_29, "b4_29"),
                (b4_30, "b4_30"),
                (a5_19, "a5_19"),
                (a5_26, "a5_26"),
                (a5_27, "a5_27"),
                (a5_29, "a5_29"),
                (a5_32, "a5_32"),
                (d5_19, "d5_19"),
                (d5_26, "d5_26"),
                (d5_27, "d5_27"),
                (d5_29, "d5_29"),
                (d5_32, "d5_32"),
                (c5_26, "c5_26"),
                (c5_27, "c5_27"),
                (c5_29, "c5_29"),
                (c5_30, "c5_30"),
                (c5_32, "c5_32"),
                (b5_29, "b5_29"),
                (b5_30, "b5_30"),
                (b5_32, "b5_32"),
                (a6_29, "a6_29"),
                (a6_32, "a6_32"),
                (d6_29, "d6_29"),
                (c6_29, "c6_29"),
                (c6_30, "c6_30"),
                (c6_32, "c6_32")
            ]
        unsatConds = filter (not . fst) conds

    putStrLn $ show (length unsatConds) ++ " / " ++
               show (length conds) ++ " unsatisfied"
    sequence_ $ map (putStrLn . ("- " ++) . snd) unsatConds


applyDifferential :: B.ByteString -> B.ByteString
applyDifferential = xorBytes $ B.concat [
        B.replicate (4 * 1) 0,
        B.pack [0, 0, 0, 0x80],
        B.pack [0, 0, 0, 0x90],
        B.replicate (4 * 9) 0,
        B.pack [0, 0, 0x01, 0],
        B.replicate (4 * 3) 0
    ]


--Note that splitBytes and concatBytes reverse endianness

splitBytes :: Word32 -> [Word8]
splitBytes x = map fromIntegral [
        (x .&. 0x000000ff),
        (x .&. 0x0000ff00) `shiftR` 8,
        (x .&. 0x00ff0000) `shiftR` 16,
        (x .&. 0xff000000) `shiftR` 24
    ]


concatBytes :: [Word8] -> Word32
concatBytes [w, x, y, z] = fromIntegral w +
    fromIntegral x `shiftL` 8 +
    fromIntegral y `shiftL` 16 +
    fromIntegral z `shiftL` 24
