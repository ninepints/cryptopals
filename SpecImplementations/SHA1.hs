-- Based on pseudocode from https://en.wikipedia.org/wiki/SHA-1
module SpecImplementations.SHA1 (
    hash,
    initContext,
    buildContext,
    update,
    finalize,
    pad
) where

import Control.Monad (guard)
import Data.Array.IArray ((!), array, elems, Array)
import Data.Bits ((.&.), (.|.), complement, rotateL, shiftL, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (unfoldr)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple (swap)
import Data.Word (Word8, Word32)
import Prelude hiding (init, rem)

import qualified Data.ByteString.Common as BC
import Data.Chunkable (chunksOf)


-- | Hash context, including a count of bytes hashed so far
-- and any "remaining" bytes that have yet to be hashed
data Context = Context {
    getState :: State,
    getByteCount :: Integer,
    getRemaining :: BL.ByteString
} deriving Show

-- | Raw hash state
data State = State Word32 Word32 Word32 Word32 Word32 deriving Show


hash :: BC.ByteString a => a -> BS.ByteString
hash = finalize . update initContext


initContext :: Context
initContext = Context (State a b c d e) 0 BL.empty
    where
        a = 0x67452301
        b = 0xEFCDAB89
        c = 0x98BADCFE
        d = 0x10325476
        e = 0xC3D2E1F0


buildContext :: BS.ByteString -> Integer -> Maybe Context
buildContext bytes byteCount = do
    state <- unpack bytes
    guard $ byteCount >= 0
    return $ Context state byteCount BL.empty


update :: BC.ByteString a => Context -> a -> Context
update (Context state cnt rem) bytes = drainChunks $ Context state cnt rem'
    where rem' = BL.append rem $ BL.fromStrict $ BC.toStrict bytes


finalize :: Context -> BS.ByteString
finalize (Context state cnt rem) = pack $ getState $ drainChunks $ newContext
    where
        newContext = Context state undefined $ pad bitCount rem
        bitCount = 8 * (cnt + fromIntegral (BL.length rem))


pad :: BC.ByteString a => Integer -> a -> a
pad bitCount bytes = BC.concat parts
    where
        parts = [bytes, BC.singleton 128, p1, p2, p3, encodedCnt]

        encodedCnt = BC.pack $ reverse $ unfoldr splitByte bitCount
        splitByte val = if val > 0
            then Just $ swap $ fmap fromIntegral $ val `divMod` 256
            else Nothing
        p1 =  BC.replicate (8 - BC.length encodedCnt) 0

        chunkMod = 1 + BC.length bytes `mod` 64
        p2 = BC.replicate (56 - chunkMod) 0
        p3 = BC.replicate (if chunkMod > 56 then 64 else 0) 0


drainChunks :: Context -> Context
drainChunks (Context state cnt rem) = Context state' cnt' rem'
    where
        chunks = chunksOf 64 rem
        isFull = (== 64) . BL.length
        (fullChunks, lastChunk) = span isFull chunks
        state' = foldl updateWithChunk state $ map BL.toStrict fullChunks
        cnt' = cnt + 64 * (fromIntegral $ length fullChunks)
        rem' = fromMaybe BL.empty $ listToMaybe lastChunk


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
xform1 subChunk state@(State _ b c d _) = _xform subChunk f 0x5A827999 state
    where f = (b .&. c) .|. ((complement b) .&. d)

xform2 :: Word32 -> State -> State
xform2 subChunk state@(State _ b c d _) = _xform subChunk f 0x6ED9EBA1 state
    where f = b `xor` c `xor` d

xform3 :: Word32 -> State -> State
xform3 subChunk state@(State _ b c d _) = _xform subChunk f 0x8F1BBCDC state
    where f = (b .&. c) .|. (b .&. d) .|. (c .&. d)

xform4 :: Word32 -> State -> State
xform4 subChunk state@(State _ b c d _) = _xform subChunk f 0xCA62C1D6 state
    where f = b `xor` c `xor` d

_xform :: Word32 -> Word32 -> Word32 -> State -> State
_xform subChunk f k (State a b c d e) = State a' b' c' d' e'
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

