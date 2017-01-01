import Data.Char (isSpace)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, IOMode(ReadMode), openFile)

import qualified ByteFormat
import qualified Vigenere as V


main = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let joinedContents = filter (not . isSpace) contents
        Just decodedContents = ByteFormat.b64ToBytes joinedContents
        candidates = V.guessVigenereKey decodedContents
    sequence $ map (putStrLn . V.showDecryption) candidates
    hClose handle
