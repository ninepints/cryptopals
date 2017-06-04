import Control.Monad (guard)
import qualified Data.ByteString as B
import Data.Function (on)
import Data.List (intercalate)

import Util (powerList, pairs)
import WeakHash (makeWeakHash, findCollision1, getAttempts,
                 getBlockA, getBlockB, getState, HashFunc, Collision)


main :: IO ()
main = do
    let f = makeWeakHash 1 0
        g = makeWeakHash 2 1

        -- How many consecutive colliding block pairs do we need in
        -- order to generate some number of colliding message pairs?
        pairsFromBlocks b = sum [1..(2^b)-1]
        blocksReqForMsgs p = head $ filter ((>= p) . pairsFromBlocks) [1..]
        nthElem l = (!!) l . flip (-) 1

        attemptsReqForMsgs hash =
            map (nthElem (attemptsReqForBlocks hash) . blocksReqForMsgs) [1..]

    putStrLn $ "Calls to f compression func required for 1k/10k/100k/1m " ++
        "pairs in f:"
    putStrLn $ intercalate "\n" $
        map (show . nthElem (attemptsReqForMsgs f) . (10 ^)) [4..7]

    putStrLn $ "Calls to g compression func required for 1k/10k/100k/1m " ++
        "pairs in g:"
    putStrLn $ intercalate "\n" $
        map (show . nthElem (attemptsReqForMsgs g) . (10 ^)) [4..7]

    putStrLn $ "Calls to (f compression func, g) required for pairs in f || g:"
    putStrLn $ intercalate "\n" $
        map (show . nthElem (attemptsReqForMsgsComposite f g)) [1..8]


findCollidingBlocks :: (Integer, HashFunc) -> (B.ByteString, [Collision])
findCollidingBlocks (stateSize, hash) = (initState, iterate next initCollision)
    where
        initState = B.replicate (fromIntegral stateSize) 0
        initCollision = findCollision1 hash initState
        next collision = findCollision1 hash $ getState collision


-- How many compression function invocations are required to generate
-- the given number of consecutive colliding block pairs? First element
-- is the invocations for one pair, second for two pairs, etc.
attemptsReqForBlocks :: (Integer, HashFunc) -> [Integer]
attemptsReqForBlocks = scanl1 (+) . map getAttempts . snd . findCollidingBlocks


attemptsReqForMsgsComposite ::
    (Integer, HashFunc) -> (Integer, HashFunc) -> [(Integer, Integer)]
attemptsReqForMsgsComposite f (stateSizeG, hashG) = result
    where
        (_, collisions) = findCollidingBlocks f
        initStateG = B.replicate (fromIntegral stateSizeG) 0

        -- Groups of colliding messages under f, plus compression
        -- function invocations required to find each group
        msgsGrouped :: [(Integer, [B.ByteString])]
        msgsGrouped = do
            i <- [0..]
            let collisionsThruI = take (i+1) collisions
                attemptsThruI = sum $ map getAttempts collisionsThruI
                messages = do
                    bools <- powerList [False, True] (fromIntegral i + 1)
                    let ab bool = if bool then getBlockB else getBlockA
                    return $ B.concat $ zipWith ab bools collisionsThruI
            return (attemptsThruI, messages)

        -- Pairs of colliding messages under f
        msgsPaired :: [(Integer, [(B.ByteString, B.ByteString)])]
        msgsPaired = map (fmap pairs) msgsGrouped

        result = do
            (attemptsF, msgPairs) <- msgsPaired
            (attemptsG, (msgA, msgB)) <- zip [1..] msgPairs
            guard $ ((==) `on` hashG initStateG) msgA msgB
            return (attemptsF, attemptsG)
