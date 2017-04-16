{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module SpecImplementations.HashCommon (
    hash,
    buildInitContext,
    buildContext,
    update,
    finalize,
    pad,
    Endianness(..),
    HashImpl(..),
    Context
) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (unfoldr)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple (swap)
import Prelude hiding (rem)

import qualified Data.ByteString.Common as BC
import Data.Chunkable (chunksOf)


data Endianness = BigEndian | LittleEndian


class Show h => HashImpl h a | h -> a where
    padEndianness :: h -> Endianness
    initState :: h -> a
    updateWithChunk :: h -> a -> BS.ByteString -> a
    pack :: h -> a -> BS.ByteString
    unpack :: h -> BS.ByteString -> Maybe a


-- | Hash context, including a count of bytes hashed so far
-- and any "remaining" bytes that have yet to be hashed
data Context a = Context {
    getState :: a,
    getByteCount :: Integer,
    getRemaining :: BL.ByteString
} deriving Show


hash :: (HashImpl h a, BC.ByteString b) => h -> b -> BS.ByteString
hash impl = finalize impl . update impl (buildInitContext impl)


buildInitContext :: HashImpl h a => h -> Context a
buildInitContext impl = Context (initState impl) 0 BL.empty


buildContext :: HashImpl h a =>
    h -> BS.ByteString -> Integer -> Maybe (Context a)
buildContext impl bytes byteCount = do
    state <- unpack impl bytes
    guard $ byteCount >= 0
    return $ Context state byteCount BL.empty


update :: (HashImpl h a, BC.ByteString b) => h -> Context a -> b -> Context a
update impl (Context state cnt rem) bytes = drainChunks impl newContext
    where
        rem' = BL.append rem $ BL.fromStrict $ BC.toStrict bytes
        newContext = Context state cnt rem'


finalize :: HashImpl h a => h -> Context a -> BS.ByteString
finalize impl (Context state cnt rem) = pack impl finalState
    where
        bitCount = 8 * (cnt + fromIntegral (BL.length rem))
        paddedRemaining = pad impl bitCount rem
        paddedContext = Context state undefined paddedRemaining
        finalState = getState $ drainChunks impl paddedContext


-- | If the given context includes 64 or more unhashed bytes,
-- updates the enclosed state with successive chunks of 64 bytes.
-- Returns a context with fewer than 64 unhashed bytes.
drainChunks :: HashImpl h a => h -> Context a -> Context a
drainChunks impl (Context state cnt rem) = Context state' cnt' rem'
    where
        chunks = chunksOf 64 rem
        isFull = (== 64) . BL.length
        (fullChunks, lastChunk) = span isFull chunks
        updateWithChunkImpl = updateWithChunk impl
        state' = foldl updateWithChunkImpl state $ map BL.toStrict fullChunks
        cnt' = cnt + 64 * (fromIntegral $ length fullChunks)
        rem' = fromMaybe BL.empty $ listToMaybe lastChunk


-- | Pads a bytestring by appending 0x80, followed by zero or more zero
-- bytes and an eight-byte representation of the given integer. The
-- length of the resulting bytestring is divisible by 64.
pad :: (HashImpl h a, BC.ByteString b) => h -> Integer -> b -> b
pad impl bitCount bytes | bitCount > 2^64 - 1 = error "> 64-bit bit count"
                        | otherwise = BC.concat parts
    where
        parts = [bytes, BC.singleton 128, zeros, reorderedCount]

        splitByte val = if val > 0
            then Just $ swap $ fmap fromIntegral $ val `divMod` 256
            else Nothing
        bigEndianCount = BC.pack $ reverse $ unfoldr splitByte bitCount
        countPadding =  BC.replicate (8 - BC.length bigEndianCount) 0

        reorder = case padEndianness impl of
            BigEndian -> id
            LittleEndian -> BC.reverse
        reorderedCount = reorder $ BC.append countPadding bigEndianCount

        chunkMod = 1 + BC.length bytes `mod` 64
        zeros = BC.replicate ((if chunkMod > 56 then 120 else 56) - chunkMod) 0
