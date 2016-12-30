module FrequencyAnalysis (scoreEnglish) where

import Data.Char (isAscii, isAlpha, isSpace, toLower)
import qualified Data.Map.Strict as Map


englishFreqs :: Map.Map Char Double
englishFreqs = Map.fromList [
        ('a', 0.08167),
        ('b', 0.01492),
        ('c', 0.02782),
        ('d', 0.04253),
        ('e', 0.12702),
        ('f', 0.02228),
        ('g', 0.02015),
        ('h', 0.06094),
        ('i', 0.06966),
        ('j', 0.00153),
        ('k', 0.00772),
        ('l', 0.04025),
        ('m', 0.02406),
        ('n', 0.06749),
        ('o', 0.07507),
        ('p', 0.01929),
        ('q', 0.00095),
        ('r', 0.05987),
        ('s', 0.06327),
        ('t', 0.09056),
        ('u', 0.02758),
        ('v', 0.00978),
        ('w', 0.02360),
        ('x', 0.00150),
        ('y', 0.01974),
        ('z', 0.00074)
    ]


isEnglishChar :: Char -> Bool
isEnglishChar = (&&) <$> isAscii <*> isAlpha

-- | Normalize the input string by removing space characters (except
-- those that appear after two or more consecutive space characters)
-- and converting to lowercase.
normalize :: String -> String
normalize = map toLower . normalizeAfterConsecutiveSpaces 0
    where
        normalizeAfterConsecutiveSpaces _ "" = ""
        normalizeAfterConsecutiveSpaces n (x:xs)
            | n < 2 && isSpace x = normalizeAfterConsecutiveSpaces (n + 1) xs
            | isSpace x = x : normalizeAfterConsecutiveSpaces n xs
            | otherwise = x : normalizeAfterConsecutiveSpaces 0 xs


getFrequencyMap :: String -> Map.Map Char Double
getFrequencyMap s = Map.map (/ letterTotal) letterCounts
    where
        -- (\x -> (x, 1)) can be (,1) if compiled with the -XTupleSections flag
        letterCounts = Map.fromListWith (+) $ map (\x -> (x, 1)) s
        letterTotal = fromIntegral $ length s


scoreEnglish :: String -> Double
scoreEnglish s = if length sNorm == 0
    then 1
    else (sum $ map distanceSquared sNorm) / (fromIntegral $ length sNorm)
    where
        sNorm = normalize s
        inputFreqs = getFrequencyMap $ filter isEnglishChar sNorm
        inputFreq c = Map.findWithDefault 1 c inputFreqs
        englishFreq c = Map.findWithDefault 0 c englishFreqs
        distanceSquared c = (inputFreq c - englishFreq c) ^ 2
