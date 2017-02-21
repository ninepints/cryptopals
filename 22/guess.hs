import Data.Maybe (catMaybes)
import Data.Word (Word32)
import System.Environment (getArgs)
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Random.Mersenne (next, seedGen)


checkGuess :: Word32 -> Word32 -> Maybe (IO ())
checkGuess output seed = if (fst $ next $ seedGen seed) == output
    then Just $ putStrLn $ "Found seed " ++ show seed
    else Nothing


main :: IO ()
main = do
    [output] <- getArgs
    now <- fmap round getPOSIXTime
    sequence_ $ catMaybes $ map (checkGuess $ read output) [now-600..now]
