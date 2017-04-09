{-# LANGUAGE TupleSections #-}

module FrequencyAnalysis (scoreEnglish) where

import Data.Char (chr, isAscii, isAlpha)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)

import qualified Data.ByteString.Common as B
import Data.Char.IntegralHelpers (isSpace', ord', toLower')


englishFreqs :: Map.Map Word8 Double
englishFreqs = Map.fromList [
        (ord' 'a', 0.08167),
        (ord' 'b', 0.01492),
        (ord' 'c', 0.02782),
        (ord' 'd', 0.04253),
        (ord' 'e', 0.12702),
        (ord' 'f', 0.02228),
        (ord' 'g', 0.02015),
        (ord' 'h', 0.06094),
        (ord' 'i', 0.06966),
        (ord' 'j', 0.00153),
        (ord' 'k', 0.00772),
        (ord' 'l', 0.04025),
        (ord' 'm', 0.02406),
        (ord' 'n', 0.06749),
        (ord' 'o', 0.07507),
        (ord' 'p', 0.01929),
        (ord' 'q', 0.00095),
        (ord' 'r', 0.05987),
        (ord' 's', 0.06327),
        (ord' 't', 0.09056),
        (ord' 'u', 0.02758),
        (ord' 'v', 0.00978),
        (ord' 'w', 0.02360),
        (ord' 'x', 0.00150),
        (ord' 'y', 0.01974),
        (ord' 'z', 0.00074)
    ]


isEnglishChar :: Word8 -> Bool
isEnglishChar = and . sequence [isAlpha, isAscii] . chr . fromIntegral

-- | Normalize the input string by removing space characters (except
-- those that appear after two or more consecutive space characters)
-- and converting to lowercase.
normalize :: B.ByteString a => a -> a
normalize = B.pack . map toLower' . normAfterConsecutiveSpaces 0 . B.unpack
    where
        normAfterConsecutiveSpaces _ [] = []
        normAfterConsecutiveSpaces n (x:xs)
            | n < 2 && isSpace' x = normAfterConsecutiveSpaces (n + 1) xs
            | isSpace' x = x : normAfterConsecutiveSpaces n xs
            | otherwise = x : normAfterConsecutiveSpaces 0 xs


getFrequencyMap :: B.ByteString a => a -> Map.Map Word8 Double
getFrequencyMap s = Map.map (/letterTotal) letterCounts
    where
        letterCounts = Map.fromListWith (+) $ map (,1) $ B.unpack s
        letterTotal = fromIntegral $ B.length s


scoreEnglish :: B.ByteString a => a -> Double
scoreEnglish s = if B.length sNorm == 0
    then 1
    else distanceSum / (fromIntegral $ B.length sNorm)
    where
        sNorm = normalize s
        inputFreqs = getFrequencyMap $ B.filter isEnglishChar sNorm
        inputFreq c = Map.findWithDefault 1 c inputFreqs
        englishFreq c = Map.findWithDefault 0 c englishFreqs
        distanceSquared c = (inputFreq c - englishFreq c) ^ 2
        distanceSum = sum $ map distanceSquared $ B.unpack sNorm
