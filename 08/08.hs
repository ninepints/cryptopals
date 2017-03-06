import qualified Data.ByteString.Char8 as B
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import ByteFormat (hexToBytes)
import Data.Chunkable (chunksOf)
import Util (uniqueness)


main :: IO ()
main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let decodedLines = map (fromJust . hexToBytes . B.pack) $ lines contents
        indexedLines = zip [1..] decodedLines
        score = uniqueness . chunksOf 16
        candidates = map (swap . fmap score) indexedLines
        topCandidates = take 5 $ sort candidates

    sequence_ $ map print topCandidates
    hClose handle
