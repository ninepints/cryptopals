import Control.Monad.State (evalState, runState, state, State)
import Data.Word (Word32)
import System.Random (randomIO)

import qualified System.Random.Mersenne as M


runFor :: Int -> State M.MersenneGen [Word32]
runFor i = sequence $ replicate i $ state M.next

main :: IO ()
main = do
    seed <- randomIO :: IO Word32

    let gen1 = M.seedGen seed
        (output, gen1') = runState (runFor $ fromIntegral M.stateLength) gen1
        gen2 = M.genFromState $ map M.untemper output

    putStrLn "Next five numbers from original generator:"
    sequence_ $ map print $ evalState (runFor 5) gen1'
    putStrLn "Next five numbers from new generator:"
    sequence_ $ map print $ evalState (runFor 5) gen2
