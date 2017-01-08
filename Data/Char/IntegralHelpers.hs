module Data.Char.IntegralHelpers where

import Data.Char (chr, ord, isSpace, toLower, toUpper)


chr' :: Integral a => a -> Char
chr' = chr . fromIntegral

ord' :: Integral a => Char -> a
ord' = fromIntegral . ord

isSpace' :: Integral a => a -> Bool
isSpace' = isSpace . chr'

toLower' :: Integral a => a -> a
toLower' = ord' . toLower . chr'

toUpper' :: Integral a => a -> a
toUpper' = ord' . toUpper . chr'
