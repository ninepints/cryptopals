{-# LANGUAGE TupleSections #-}

import Data.ByteString.Char8 (pack, ByteString)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Environment (getArgs)

import ByteFormat (hexToBytes)
import qualified Vigenere as V


type IndexedGuess = (Integer, V.KeyGuess ByteString Word8)


toIndexedGuesses :: (Integer, ByteString) -> [IndexedGuess]
toIndexedGuesses (i, ciphertext) = map (i,) guesses
    where guesses = V.guessSingleByteKey ciphertext


showIndexedGuess :: IndexedGuess -> String
showIndexedGuess c = "Line " ++ show (fst c) ++ ": " ++ V.showKeyGuess (snd c)


main :: IO ()
main = do
    [filename] <- getArgs
    contents <- readFile filename

    let decodedLines = map (fromJust . hexToBytes . pack) $ lines contents
        indexedLines = zip [1..] decodedLines
        guesses = indexedLines >>= toIndexedGuesses
        topGuesses = take 5 $ sortBy (compare `on` (V.getScore . snd)) guesses

    sequence_ $ map (putStrLn . showIndexedGuess) topGuesses
