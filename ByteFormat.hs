-- There are almost certainly packages for this (and I'm kinda cheating
-- on the hex encoding) but I think the point is to do it by hand

module ByteFormat (
    hexToBytes,
    bytesToHex,
    b64ToBytes,
    bytesToB64,
    hexToB64,
    b64ToHex,
    urlEscape,
    urlEscapeChars
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord, toUpper)
import qualified Data.Map.Strict as Map
import Data.String (fromString, IsString)
import Data.Tuple (swap)
import Data.Word (Word8)
import Numeric (readHex, showHex)

import qualified Data.ByteString.Common as B
import Data.Chunkable (chunksOf)


-- | Map of integers 0-63 to corresponding base-64 characters.
b64IntToChar :: Map.Map Word8 Char
b64IntToChar = Map.fromList $
    (map (\x -> (fromIntegral x, chr $ ord 'A' + x)) [0..25]) ++
    (map (\x -> (fromIntegral x, chr $ ord 'a' + x - 26)) [26..51]) ++
    (map (\x -> (fromIntegral x, chr $ ord '0' + x - 52)) [52..61]) ++
    [(62, '+'), (63, '/')]

-- | Map of base-64 characters to integers.
b64CharToInt :: Map.Map Char Word8
b64CharToInt = Map.fromList (('=', 0) : map swap (Map.toList b64IntToChar))


hexToBytes :: B.ByteString a => String -> Maybe a
hexToBytes = fmap B.pack . sequence . map charPairToByte . chunksOf 2
    where
        charPairToByte h@([_,_]) = case readHex h of
            [(i,"")] -> Just i
            _ -> Nothing
        charPairToByte _ = Nothing


bytesToHex :: B.ByteString a => a -> String
bytesToHex bytes = B.unpack bytes >>= toHex
    where toHex i | i < 16 = '0' : showHex i ""
          toHex i | otherwise = showHex i ""


b64ToBytes :: B.ByteString a => String -> Maybe a
b64ToBytes = fmap pack . sequence . map charQuadToBytes . chunksOf 4
    where
        pack = B.pack . concat
        lookup = (b64CharToInt Map.!)

        charQuadToBytes [a,b,'=','='] = let
            x1 = lookup a
            (x2,y1) = quotRem (lookup b) 16
            in if y1 == 0 then Just [4 * x1 + x2] else Nothing
        charQuadToBytes [a,b,c,'='] = let
            x1 = lookup a
            (x2,y1) = quotRem (lookup b) 16
            (y2,z1) = quotRem (lookup c) 4
            in if z1 == 0 then Just [4 * x1 + x2, 16 * y1 + y2] else Nothing
        charQuadToBytes [a,b,c,d] = let
            x1 = lookup a
            (x2,y1) = quotRem (lookup b) 16
            (y2,z1) = quotRem (lookup c) 4
            z2 = lookup d
            in Just [4 * x1 + x2, 16 * y1 + y2, 64 * z1 + z2]
        charQuadToBytes _ = Nothing


bytesToB64 :: B.ByteString a => a -> String
bytesToB64 bytes = chunksOf 3 bitPairs >>= bitPairsToB64
    where
        bitPairs = B.unpack bytes >>= bytesToBitPairs
        lookup = (:[]) . (b64IntToChar Map.!)
        bytesToBitPairs x = [a,b,c,d]
            where
                (r1,d) = quotRem x 4
                (r2,c) = quotRem r1 4
                (a,b) = quotRem r2 4
        bitPairsToB64 [x,y,z] = lookup (16 * x + 4 * y + z)
        bitPairsToB64 [x,y] = lookup (16 * x + 4 * y) ++ "="
        bitPairsToB64 [x] = lookup (16 * x) ++ "=="


hexToB64 :: String -> Maybe String
hexToB64 = fmap (bytesToB64 :: BL.ByteString -> String) . hexToBytes


b64ToHex :: String -> Maybe String
b64ToHex = fmap (bytesToHex :: BL.ByteString -> String) . b64ToBytes


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
        definitelyEscape = fromString . map toUpper . bytesToHex . BS.singleton

