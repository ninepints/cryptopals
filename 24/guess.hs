import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes)
import Data.Word (Word16)
import System.Environment (getArgs)

import System.Random.Mersenne (sketchyCtr)


plaintext :: B.ByteString
plaintext = B.pack "AAAAAAAAAAAAAA"


checkGuess :: B.ByteString -> Word16 -> Maybe (IO ())
checkGuess ciphertext guess = if plaintext' == plaintext
    then Just $ putStrLn $ "Found seed " ++ show guess
    else Nothing
    where
        plaintext' = B.drop (B.length ciphertext - B.length plaintext) $
            sketchyCtr (fromIntegral guess) ciphertext


main :: IO ()
main = do
    [arg] <- getArgs
    let ciphertext = B.pack $ read arg
    sequence_ $ catMaybes $ map (checkGuess ciphertext) [minBound..maxBound]
