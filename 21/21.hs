import Control.Monad.State (evalState, state, State)
import Data.Word (Word32)

import qualified System.Random.Mersenne as M


next :: State M.MersenneGen Word32
next = state M.next

runFor :: Int -> State M.MersenneGen [Word32]
runFor i = sequence $ replicate i next

main :: IO ()
main = sequence_ $ map print $ evalState (runFor 630) (M.seedGen 5)
