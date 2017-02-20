import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, IOMode(ReadMode), openFile)

import qualified ByteFormat
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
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let splitContents = lines contents
        decode = fromJust . ByteFormat.hexToBytes . fromString
        decodedContents = map decode splitContents
        indexedContents = zip [1..] decodedContents
        candidates = indexedContents >>= toIndexedCandidates
        topCandidates = take 5 $ sortBy indexedCandidateCmp candidates

    sequence_ $ map (putStrLn . showIndexedCandidate) topCandidates
    hClose handle
