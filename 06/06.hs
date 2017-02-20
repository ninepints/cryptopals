import Data.ByteString.Lazy (ByteString)
import Data.Char (isSpace)
import Data.String (fromString)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, IOMode(ReadMode), openFile)

import qualified ByteFormat
import qualified Vigenere as V


main :: IO ()
main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle

    let joinedContents :: ByteString
        joinedContents = fromString $ filter (not . isSpace) contents
        Just decodedContents = ByteFormat.base64ToBytes joinedContents
        candidates = V.guessVigenereKey decodedContents

    sequence_ $ map (putStrLn . V.showSolution) candidates
    hClose handle
