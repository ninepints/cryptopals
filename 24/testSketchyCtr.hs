import qualified Data.ByteString.Char8 as B
import Data.Word (Word32)
import System.Random (randomIO)

import System.Random.Mersenne (sketchyCtr)


main :: IO ()
main = do
    seed <- randomIO :: IO Word32

    let plaintext = B.pack "Hey here's some plaintext!"
        ciphertext = sketchyCtr seed plaintext
        plaintext' = sketchyCtr seed ciphertext

    putStrLn "The plaintext:"
    print plaintext
    putStrLn "Sketchy CTR ciphertext:"
    print ciphertext
    putStrLn "And back again:"
    print plaintext'
