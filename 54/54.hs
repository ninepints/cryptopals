import Control.Applicative (liftA2)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (unfoldr)
import Text.Printf (printf)

import ByteFormat (integerToBytes)
import Data.Chunkable (chunksOf)
import Padding (constantPad, constantPadLeft)
import WeakHash (blockSize, makeWeakHash, findCollision2, HashFunc)
import qualified WeakHash as C (Collision(..))


data Node = Root { getState :: B.ByteString } |
            Branch {
                getState :: B.ByteString,
                getBlock :: B.ByteString,
                getParent :: Node
            }
            deriving Show


k :: Integer
k = 8


stateSize :: Integer
hash :: HashFunc
(stateSize, hash) = makeWeakHash 3 0

initState :: B.ByteString
initState = B.replicate (fromIntegral stateSize) 0


main :: IO ()
main = do
    let isRoot (Root _) = True
        isRoot _ = False
        root = until isRoot getParent $ head leaves

    putStrLn $ printf "Committing to hash %s and length %u"
        (show $ getState root) (blockSize * (k + 2))

    let inputBlocks = constantPad blockSize 0 $ BC.pack "baseball stuff"
        stateAfterInput = hash initState inputBlocks
        integerToBlock = constantPadLeft blockSize 0 . integerToBytes
        (bridgeAttempts, (bridgeBlock, stateAfterBridge)) = head $
            filter ((<= (getState $ last leaves)) . snd . snd) $
            zip [1..] $
            map (liftA2 (,) id $ hash stateAfterInput) $
            map integerToBlock [0..]

        funnelStart = head $ filter ((==) stateAfterBridge . getState) leaves
        funnelBlocks = unfoldr nextInFunnel funnelStart
            where
                nextInFunnel (Branch _ block parent) = Just (block, parent)
                nextInFunnel (Root _) = Nothing

        message = B.concat $ [inputBlocks, bridgeBlock] ++ funnelBlocks

    putStrLn $ "Message length is " ++ show (B.length message)
    putStrLn $ "Message hash is " ++ show (hash initState message)
    putStrLn $ "Funnel compression function invocations: " ++ show treeAttempts
    putStrLn $ "Bridge compression function invocations: " ++
        show bridgeAttempts


leaves :: [Node]
treeAttempts :: Integer
(leaves, treeAttempts) = buildNodes $ map integerToState [0..2^k-1]
    where integerToState = constantPadLeft stateSize 0 . integerToBytes


buildNodes :: [B.ByteString] -> ([Node], Integer)
buildNodes [state] = ([Root state], 0)
buildNodes states = (
        zipWith3 Branch states
            (collisions >>= unpackCollision)
            (parents >>= replicate 2),
        collisionAttempts + parentAttempts
    )
    where
        collidePair [stateA, stateB] = findCollision2 hash stateA stateB
        collisions = map collidePair $ chunksOf 2 states
        (parents, parentAttempts) = buildNodes $ map C.getState collisions
        collisionAttempts = sum $ map C.getAttempts collisions
        unpackCollision (C.Collision _ blockA blockB _) = [blockA, blockB]
