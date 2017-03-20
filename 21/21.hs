import Control.Monad.State (evalState, state, State)
import Data.Word (Word32)

import qualified SpecImplementations.Mersenne as M


runFor :: Int -> State M.MersenneGen [Word32]
runFor i = sequence $ replicate i $ state M.next

main :: IO ()
main = sequence_ $ map print $ evalState (runFor 630) (M.seedGen 5)
