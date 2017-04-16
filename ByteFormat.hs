-- There are almost certainly packages for this
-- but I think the point is to do it by hand

module ByteFormat (
    hexToBytes,
    bytesToHex,
    base64ToBytes,
    bytesToBase64,
    integerToBytes,
    urlEscape,
    urlEscapeChars
) where

import Control.Monad (guard, liftM2)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.Word (Word8)

import qualified Data.ByteString.Common as B
import qualified Data.ByteString.Char8 as BChar
import Data.Char.IntegralHelpers (ord', toLower', toUpper')
import Data.Chunkable (chunksOf)


eq :: Word8
eq = ord' '='


-- | Map of integers 0-63 to corresponding base-64 characters.
base64Encoding :: Map.Map Word8 Word8
base64Encoding = Map.fromList $
    (map (\x -> (x, ord' 'A' + x)) [0..25]) ++
    (map (\x -> (x, ord' 'a' + x - 26)) [26..51]) ++
    (map (\x -> (x, ord' '0' + x - 52)) [52..61]) ++
    [(62, ord' '+'), (63, ord' '/')]

-- | Map of base-64 characters to integers.
base64Decoding :: Map.Map Word8 Word8
base64Decoding = Map.fromList $ (eq, 0) : reverseEncoding
    where reverseEncoding = map swap $ Map.toList base64Encoding

-- | Map of integers 0-15 to corresponding hex characters.
hexEncoding :: Map.Map Word8 Word8
hexEncoding = Map.fromList $
    (map (\x -> (x, ord' '0' + x)) [0..9]) ++
    (map (\x -> (x, ord' 'a' + x - 10)) [10..15])

-- | Map of hex characters to integers.
hexDecoding :: Map.Map Word8 Word8
hexDecoding = Map.fromList $ map swap $ Map.toList hexEncoding


hexToBytes :: B.ByteString a => a -> Maybe a
hexToBytes input = fmap B.pack $ sequence bytes
    where
        bytes = map pairToByte $ chunksOf 2 $ B.unpack input
        decode char = Map.lookup (toLower' char) hexDecoding

        pairToByte :: [Word8] -> Maybe Word8
        pairToByte [a,b] = let
            x1 = decode a
            x2 = decode b
            in liftM2 (+) (fmap (* 16) x1) x2
        pairToByte _ = Nothing


bytesToHex :: B.ByteString a => a -> a
bytesToHex = B.concatMap byteToHex
    where
        encode = (Map.!) hexEncoding
        byteToHex byte = B.pack [encode a, encode b]
            where (a, b) = byte `divMod` 16


base64ToBytes :: B.ByteString a => a -> Maybe a
base64ToBytes input = fmap (B.pack . concat) $ sequence bytes
    where
        charQuads = chunksOf 4 $ B.unpack input
        bytes =  map charQuadToBytes charQuads
        decode char = Map.lookup char base64Decoding

        charQuadToBytes :: [Word8] -> Maybe [Word8]
        charQuadToBytes [a,b,c,d]
            | c == eq && d == eq = do
                x1 <- decode a
                x2y1 <- decode b
                let (x2, y1) = quotRem x2y1 16
                guard (y1 == 0) >> return [4 * x1 + x2]
            | d == eq = do
                x1 <- decode a
                x2y1 <- decode b
                y2z1 <- decode c
                let (x2, y1) = quotRem x2y1 16
                    (y2, z1) = quotRem y2z1 4
                guard (z1 == 0) >> return [4 * x1 + x2, 16 * y1 + y2]
            | otherwise = do
                x1 <- decode a
                x2y1 <- decode b
                y2z1 <- decode c
                z2 <- decode d
                let (x2, y1) = quotRem x2y1 16
                    (y2, z1) = quotRem y2z1 4
                return [4 * x1 + x2, 16 * y1 + y2, 64 * z1 + z2]
        charQuadToBytes _ = Nothing


bytesToBase64 :: B.ByteString a => a -> a
bytesToBase64 bytes = B.pack base64Chars
    where
        base4 = B.unpack bytes >>= byteToBase4
        base64Chars = chunksOf 3 base4 >>= base4To64

        encode = (Map.!) base64Encoding
        byteToBase4 byte = [a,b,c,d]
            where
                (r1,d) = quotRem byte 4
                (r2,c) = quotRem r1 4
                (a,b) = quotRem r2 4
        base4To64 [x,y,z] = [encode (16 * x + 4 * y + z)]
        base4To64 [x,y] = [encode (16 * x + 4 * y), eq]
        base4To64 [x] = [encode (16 * x), eq, eq]


-- | Return a big-endian representation of a nonnegative integer.
--
-- >>> integerToBytes 24936
-- "ah"
-- >>> integerToBytes 0
-- ""
integerToBytes :: B.ByteString a => Integer -> a
integerToBytes n | n < 0 = error "Input negative"
                 | n == 0 = B.empty
                 | n < 256 = B.singleton $ fromIntegral n
                 | otherwise = B.cons (fromIntegral first) $ integerToBytes rest
    where (first, rest) = n `divMod` 256


urlEscape :: B.ByteString a => a -> a
urlEscape = urlEscapeChars $ B.fromStrict $ BChar.pack "!*'();:@&=+$,/?#[]"

-- | URL-escape the second argument, only replacing bytes that are
-- present in the first.
urlEscapeChars :: B.ByteString a => a -> a -> a
urlEscapeChars escapeBytes = B.concatMap escape
    where
        percent = B.fromStrict $ BChar.singleton '%'
        escape byte = if byte `B.elem` escapeBytes
            then B.append percent $ definitelyEscape byte
            else B.singleton byte
        definitelyEscape = B.map toUpper' . bytesToHex . B.singleton
