import qualified Data.ByteString as B
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Tuple (swap)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import qualified ByteFormat
import Data.Chunkable (chunksOf)
import Util (uniqueness)


main :: IO ()
main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let decode :: String -> B.ByteString
        decode = fromJust . ByteFormat.hexToBytes . fromString
        decodedContents = map decode $ lines contents
        indexedContents = zip [1..] decodedContents
        score = uniqueness . chunksOf 16
        candidates = map (swap . fmap score) indexedContents
        topCandidates = take 5 $ sort candidates

    sequence_ $ map print topCandidates
    hClose handle
