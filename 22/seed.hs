import Data.Time.Clock.POSIX (getPOSIXTime)

import SpecImplementations.Mersenne (next, seedGen)


main :: IO ()
main = do
    -- This would be harder (but still feasible) to guess
    -- if we didn't round to the nearest second
    now <- fmap round getPOSIXTime
    putStrLn $ "The top secret seed is " ++ show now
    let gen = seedGen now
        output = fst $ next gen
    putStrLn $ "The first 32 bits of output are " ++ show output
