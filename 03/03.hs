import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.List (sortBy)
import Data.String (fromString)

import qualified ByteFormat
import qualified Vigenere as V


input :: ByteString
Just input = ByteFormat.hexToBytes $ fromString $
    "1b37373331363f78151b7f2b783431333d" ++
    "78397828372d363c78373e783a393b3736"

main :: IO ()
main = sequence_ $ map (putStrLn . V.showSolution) topCandidates
    where
        candidates = V.guessSingleByteKey input
        topCandidates = take 5 $ sortBy (compare `on` V.score) candidates
