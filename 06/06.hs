import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)

import ByteFormat (base64ToBytes)
import qualified Vigenere as V


main :: IO ()
main = do
    [filename] <- getArgs
    contents <- readFile filename

    let Just decodedContents = base64ToBytes $ pack $ concat $ lines contents
        guesses = V.guessVigenereKey decodedContents

    sequence_ $ map (putStrLn . V.showKeyGuess) guesses
