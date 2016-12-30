import Data.Char (isSpace)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, IOMode(ReadMode), openFile)

import qualified ByteFormat
import qualified Vigenere as V


main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    let joinedContents = filter (not . isSpace) contents
        decodedContents = fromJust $ ByteFormat.b64ToBytes joinedContents
        candidates = V.guessVigenereKey decodedContents
    sequence $ map (putStrLn . V.showDecryption) candidates
    hClose handle
