-- There are almost certainly packages for this
-- but I think the point is to do it by hand

module ByteFormat (
    hexToBytes,
    bytesToHex,
    base64ToBytes,
    bytesToBase64,
    hexToBase64,
    base64ToHex,
    urlEscape,
    urlEscapeChars
) where

import qualified Data.Map.Strict as Map
import Data.String (fromString, IsString)
import Data.Tuple (swap)
import Data.Word (Word8)

import qualified Data.ByteString.Common as B
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
hexToBytes = fmap B.pack . sequence . map charPairToByte . chunksOf 2 . B.unpack
    where
        decode = (Map.!) hexDecoding . toLower'
        charPairToByte [a,b] = let
            x1 = decode a
            x2 = decode b
            in Just $ 16 * x1 + x2
        charPairToByte _ = Nothing


bytesToHex :: B.ByteString a => a -> a
bytesToHex = B.concatMap byteToHex
    where
        encode = (Map.!) hexEncoding
        byteToHex byte = B.pack [encode a, encode b]
            where (a, b) = byte `divMod` 16


base64ToBytes :: B.ByteString a => a -> Maybe a
base64ToBytes = fmap (pack) . sequence . map charQuadToBytes . chunksOf 4 . B.unpack
    where
        pack = B.pack . concat
        decode = (Map.!) base64Decoding

        charQuadToBytes [a,b,c,d]
            | c == eq && d == eq = let
                x1 = decode a
                (x2,y1) = quotRem (decode b) 16
                in if y1 == 0 then Just [4 * x1 + x2] else Nothing
            | c == eq = let
                x1 = decode a
                (x2,y1) = quotRem (decode b) 16
                (y2,z1) = quotRem (decode c) 4
                in if z1 == 0 then Just [4 * x1 + x2, 16 * y1 + y2] else Nothing
            | otherwise = let
                x1 = decode a
                (x2,y1) = quotRem (decode b) 16
                (y2,z1) = quotRem (decode c) 4
                z2 = decode d
                in Just [4 * x1 + x2, 16 * y1 + y2, 64 * z1 + z2]
        charQuadToBytes _ = Nothing


bytesToBase64 :: B.ByteString a => a -> a
bytesToBase64 bytes = B.pack $ chunksOf 3 bitPairs >>= bitPairsToB64
    where
        bitPairs = B.unpack bytes >>= bytesToBitPairs
        encode = (Map.!) base64Encoding
        bytesToBitPairs x = [a,b,c,d]
            where
                (r1,d) = quotRem x 4
                (r2,c) = quotRem r1 4
                (a,b) = quotRem r2 4
        bitPairsToB64 [x,y,z] = [encode (16 * x + 4 * y + z)]
        bitPairsToB64 [x,y] = [encode (16 * x + 4 * y), eq]
        bitPairsToB64 [x] = [encode (16 * x), eq, eq]


hexToBase64 :: B.ByteString a => a -> Maybe a
hexToBase64 = fmap bytesToBase64 . hexToBytes


base64ToHex :: B.ByteString a => a -> Maybe a
base64ToHex = fmap bytesToHex . base64ToBytes


urlEscape :: (B.ByteString a, IsString a) => a -> a
urlEscape = urlEscapeChars $ fromString "!*'();:@&=+$,/?#[]"

-- | URL-escape the second argument, only replacing bytes that are
-- present in the first.
urlEscapeChars :: (B.ByteString a, IsString a) => a -> a -> a
urlEscapeChars escapeBytes = B.concatMap escape
    where
        escape byte = if byte `B.elem` escapeBytes
            then B.append (fromString "%") $ definitelyEscape byte
            else B.singleton byte
        definitelyEscape = B.map toUpper' . bytesToHex . B.singleton

