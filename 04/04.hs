import Data.ByteString.Char8 (pack, ByteString)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Environment (getArgs)

import ByteFormat (hexToBytes)
import qualified Vigenere as V


type IndexedCandidate = (Integer, V.Solution ByteString Word8)


toIndexedCandidates :: (Integer, ByteString) -> [IndexedCandidate]
toIndexedCandidates (i, ciphertext) = map (\can -> (i, can)) candidates
    where candidates = V.guessSingleByteKey ciphertext


indexedCandidateCmp :: IndexedCandidate -> IndexedCandidate -> Ordering
indexedCandidateCmp = compare `on` (V.score . snd)


showIndexedCandidate :: IndexedCandidate -> String
showIndexedCandidate c = "Line " ++ show (fst c) ++ ": " ++
    V.showSolution (snd c)


main :: IO ()
main = do
    [filename] <- getArgs
    contents <- readFile filename

    let decodedLines = map (fromJust . hexToBytes . pack) $ lines contents
        indexedLines = zip [1..] decodedLines
        candidates = indexedLines >>= toIndexedCandidates
        topCandidates = take 5 $ sortBy indexedCandidateCmp candidates

    sequence_ $ map (putStrLn . showIndexedCandidate) topCandidates
