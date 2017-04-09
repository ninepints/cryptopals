-- Based on https://tools.ietf.org/html/rfc1320

{-# LANGUAGE MultiParamTypeClasses #-}

module SpecImplementations.MD4 (
    hash,
    initContext,
    buildContext,
    update,
    finalize,
    MD4(..)
) where

import Control.Monad (guard)
import Data.Array.IArray ((!), array, Array)
import Data.Bits ((.&.), (.|.), complement, rotateL, shiftL, shiftR, xor)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

import qualified Data.ByteString.Common as BC
import Data.Chunkable (chunksOf)
import qualified SpecImplementations.HashCommon as HC


data MD4 = MD4 deriving Show
data State = State Word32 Word32 Word32 Word32 deriving Show
type Context = HC.Context State

instance HC.HashImpl MD4 State where
    padEndianness _ = HC.LittleEndian
    initState _ = initState
    updateWithChunk _ = updateWithChunk
    pack _ = pack
    unpack _ = unpack


hash :: (BC.ByteString a) => a -> BS.ByteString
hash = HC.hash MD4

initContext :: Context
initContext = HC.buildInitContext MD4

buildContext ::  BS.ByteString -> Integer -> Maybe Context
buildContext = HC.buildContext MD4

update :: (BC.ByteString a) => Context -> a -> Context
update = HC.update MD4

finalize :: Context -> BS.ByteString
finalize = HC.finalize MD4


initState :: State
initState = State 0x67452301 0xefcdab89 0x98badcfe 0x10325476


updateWithChunk :: State -> BS.ByteString -> State
updateWithChunk state@(State a b c d) chunk = newState
    where
        subChunks = map concatBytes $ chunksOf 4 $ BS.unpack chunk
        subChunksArr :: Array Word8 Word32
        subChunksArr = array (0, 15) $ zip [0..] subChunks

        xforms = [xform1, xform2, xform3] >>= replicate 16

        shifts = [
                [3, 7, 11, 19],
                [3, 5, 9, 13],
                [3, 9, 11, 15]
            ] >>= replicate 4 >>= id

        indices = [0..15] ++
            [0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15] ++
            [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15]

        uncurry3 f (a, b, c) = f a b c
        permutations = map uncurry3 $ cycle [abcd, dabc, cdab, bcda]
        permutationArgs = zip3 xforms shifts $ map ((!) subChunksArr) indices
        stateUnaryOps = zipWith ($) permutations permutationArgs

        State a' b' c' d' = foldr1 (flip (.)) stateUnaryOps state
        newState = State (a + a') (b + b') (c + c') (d + d')


type TernaryOp = Word32 -> Word32 -> Word32 -> Word32
type QuarternaryOp = Word32 -> Word32 -> Word32 -> Word32 -> Word32

-- An "a" that depends on a shift size and a sub-chunk
type SSCDependent a = Int -> Word32 -> a


f :: TernaryOp
f x y z = (x .&. y) .|. ((complement x) .&. z)

g :: TernaryOp
g x y z = (x .&. y) .|.  (x .&. z) .|. (y .&. z)

h :: TernaryOp
h x y z = x `xor` y `xor` z


xform1 :: SSCDependent QuarternaryOp
xform1 = xform' f 0

xform2 :: SSCDependent QuarternaryOp
xform2 = xform' g 0x5a827999

xform3 :: SSCDependent QuarternaryOp
xform3 = xform' h 0x6ed9eba1

xform' :: TernaryOp -> Word32 -> SSCDependent QuarternaryOp
xform' ternary k shift subChunk a b c d = total `rotateL` shift
    where total = a + ternary b c d + subChunk + k


abcd :: SSCDependent QuarternaryOp -> SSCDependent (State -> State)
abcd sscdQuarternary shift subChunk (State a b c d) = State a' b c d
    where a' = sscdQuarternary shift subChunk a b c d

dabc :: SSCDependent QuarternaryOp -> SSCDependent (State -> State)
dabc sscdQuarternary shift subChunk (State a b c d) = State a b c d'
    where d' = sscdQuarternary shift subChunk d a b c

cdab :: SSCDependent QuarternaryOp -> SSCDependent (State -> State)
cdab sscdQuarternary shift subChunk (State a b c d) = State a b c' d
    where c' = sscdQuarternary shift subChunk c d a b

bcda :: SSCDependent QuarternaryOp -> SSCDependent (State -> State)
bcda sscdQuarternary shift subChunk (State a b c d) = State a b' c d
    where b' = sscdQuarternary shift subChunk b c d a


--Note that pack, unpack, and concatBytes reverse endianness

pack :: State -> BS.ByteString
pack (State a b c d) = BS.pack $ [a, b, c, d] >>= splitBytes
    where splitBytes x = map fromIntegral [
                (x .&. 0x000000ff),
                (x .&. 0x0000ff00) `shiftR` 8,
                (x .&. 0x00ff0000) `shiftR` 16,
                (x .&. 0xff000000) `shiftR` 24
            ]


unpack :: BS.ByteString -> Maybe State
unpack bytes = guard (BS.length bytes == 16) >> return (State a b c d)
    where
        a = concatBytes $ take 4 $ BS.unpack bytes
        b = concatBytes $ take 4 $ drop 4 $ BS.unpack bytes
        c = concatBytes $ take 4 $ drop 8 $ BS.unpack bytes
        d = concatBytes $ take 4 $ drop 12 $ BS.unpack bytes


concatBytes :: [Word8] -> Word32
concatBytes [w, x, y, z] = fromIntegral w +
    fromIntegral x `shiftL` 8 +
    fromIntegral y `shiftL` 16 +
    fromIntegral z `shiftL` 24
