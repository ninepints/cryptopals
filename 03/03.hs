import Data.Function (on)
import Data.List (sortBy)

import qualified ByteFormat
import qualified Vigenere as V


input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
Just ciphertext = ByteFormat.hexToBytes input
candidates = V.guessSingleByteKey ciphertext
topCandidates = take 5 $ sortBy (compare `on` V.score) candidates
main = sequence $ map (putStrLn . V.showDecryption) topCandidates
