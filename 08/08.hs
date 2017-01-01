import qualified Data.ByteString.Lazy as B
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import qualified ByteFormat
import Util (chunksOf)


-- | Score a bytestring based on fraction of chunks that are unique.
score :: Int -> B.ByteString -> Double
score size bytes = uniqueCount / chunkCount
    where
        chunks = chunksOf size bytes
        chunkCount = fromIntegral $ length chunks
        uniqueCount = fromIntegral $ Set.size $ Set.fromList chunks


main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let splitContents = lines contents
        decodedContents = map (fromJust . ByteFormat.hexToBytes) splitContents
        candidates = map (swap . fmap (score 16)) $ zip [1..] decodedContents
        topCandidates = take 5 $ sort candidates

    sequence $ map (putStrLn . show) $ topCandidates
    hClose handle
