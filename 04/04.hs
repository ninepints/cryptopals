import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, IOMode(ReadMode), openFile)

import qualified ByteFormat
import qualified Vigenere as V


toIndexedCandidates (i, ciphertext) = map (\can -> (i, can)) candidates
    where candidates = V.guessSingleByteKey ciphertext

indexedCandidateCmp = compare `on` (V.score . snd)

showIndexedCandidate c = "Line " ++ show (fst c) ++ ": " ++
    V.showDecryption (snd c)


main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    let splitContents = lines contents
        decodedContents = map (fromJust . ByteFormat.hexToBytes) splitContents
        indexedContents = zip [1..] decodedContents
        candidates = indexedContents >>= toIndexedCandidates
        topCandidates = take 5 $ sortBy indexedCandidateCmp candidates
    sequence $ map (putStrLn . showIndexedCandidate) topCandidates
    hClose handle
