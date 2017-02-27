import qualified Data.ByteString.Lazy as B
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Word (Word16)
import System.Environment (getArgs)

import System.Random.Mersenne (sketchyCtr)


plaintext :: B.ByteString
plaintext = fromString "AAAAAAAAAAAAAA"


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
    let ciphertext = fromString $ read arg :: B.ByteString
    sequence_ $ catMaybes $ map (checkGuess ciphertext) [minBound..maxBound]
