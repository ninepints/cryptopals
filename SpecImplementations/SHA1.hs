-- Based on pseudocode from <https://en.wikipedia.org/wiki/SHA-1>

{-# LANGUAGE MultiParamTypeClasses #-}

module SpecImplementations.SHA1 (
    hash,
    initContext,
    buildContext,
    update,
    finalize,
    SHA1(..)
) where

import Control.Monad (guard)
import Data.Array.IArray ((!), array, elems, Array)
import Data.Bits ((.&.), (.|.), complement, rotateL, shiftL, shiftR, xor)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

import qualified Data.ByteString.Common as BC
import Data.Chunkable (chunksOf)
import qualified SpecImplementations.HashCommon as HC


data SHA1 = SHA1 deriving Show
data State = State Word32 Word32 Word32 Word32 Word32 deriving Show
type Context = HC.Context State

instance HC.HashImpl SHA1 State where
    padEndianness _ = HC.BigEndian
    initState _ = initState
    updateWithChunk _ = updateWithChunk
    pack _ = pack
    unpack _ = unpack


hash :: BC.ByteString a => a -> BS.ByteString
hash = HC.hash SHA1

initContext :: Context
initContext = HC.buildInitContext SHA1

buildContext ::  BS.ByteString -> Integer -> Maybe Context
buildContext = HC.buildContext SHA1

update :: BC.ByteString a => Context -> a -> Context
update = HC.update SHA1

finalize :: Context -> BS.ByteString
finalize = HC.finalize SHA1


initState :: State
initState = State 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0xc3d2e1f0


updateWithChunk :: State -> BS.ByteString -> State
updateWithChunk state@(State a b c d e) chunk = newState
    where
        subChunks :: Array Word8 Word32
        subChunks = array (0, 79) $ zip [0..15] firstSubChunks ++ lastSubChunks
        firstSubChunks = map concatBytes $ chunksOf 4 $ BS.unpack chunk
        lastSubChunks = map ((,) <$> id <*> subChunkAt) [16..79]
        subChunkAt i = (
                subChunks ! (i-3) `xor`
                subChunks ! (i-8) `xor`
                subChunks ! (i-14) `xor`
                subChunks ! (i-16)
            ) `rotateL` 1

        xforms = [xform1, xform2, xform3, xform4] >>= replicate 20
        subChunksAndXforms = zip (elems subChunks) xforms
        applyXform curState (subChunk, xform) = xform subChunk curState
        State a' b' c' d' e' = foldl applyXform state subChunksAndXforms
        newState = State (a + a') (b + b') (c + c') (d + d') (e + e')


xform1 :: Word32 -> State -> State
xform1 subChunk state@(State _ b c d _) = xform' subChunk f 0x5a827999 state
    where f = (b .&. c) .|. ((complement b) .&. d)

xform2 :: Word32 -> State -> State
xform2 subChunk state@(State _ b c d _) = xform' subChunk f 0x6ed9eba1 state
    where f = b `xor` c `xor` d

xform3 :: Word32 -> State -> State
xform3 subChunk state@(State _ b c d _) = xform' subChunk f 0x8f1bbcdc state
    where f = (b .&. c) .|. (b .&. d) .|. (c .&. d)

xform4 :: Word32 -> State -> State
xform4 subChunk state@(State _ b c d _) = xform' subChunk f 0xca62c1d6 state
    where f = b `xor` c `xor` d

xform' :: Word32 -> Word32 -> Word32 -> State -> State
xform' subChunk f k (State a b c d e) = State a' b' c' d' e'
    where
        a' = (a `rotateL` 5) + e + subChunk + f + k
        b' = a
        c' = b `rotateL` 30
        d' = c
        e' = d


pack :: State -> BS.ByteString
pack (State a b c d e) = BS.pack $ [a, b, c, d, e] >>= splitBytes
    where splitBytes x = map fromIntegral [
                (x .&. 0xff000000) `shiftR` 24,
                (x .&. 0x00ff0000) `shiftR` 16,
                (x .&. 0x0000ff00) `shiftR` 8,
                (x .&. 0x000000ff)
            ]


unpack :: BS.ByteString -> Maybe State
unpack bytes = guard (BS.length bytes == 20) >> return (State a b c d e)
    where
        a = concatBytes $ take 4 $ BS.unpack bytes
        b = concatBytes $ take 4 $ drop 4 $ BS.unpack bytes
        c = concatBytes $ take 4 $ drop 8 $ BS.unpack bytes
        d = concatBytes $ take 4 $ drop 12 $ BS.unpack bytes
        e = concatBytes $ take 4 $ drop 16 $ BS.unpack bytes


concatBytes :: [Word8] -> Word32
concatBytes [w, x, y, z] = fromIntegral z +
    fromIntegral y `shiftL` 8 +
    fromIntegral x `shiftL` 16 +
    fromIntegral w `shiftL` 24
