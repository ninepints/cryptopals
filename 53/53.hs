import Data.Bits (testBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

import Data.Chunkable (chunksOf)
import WeakHash (blockSize, makeWeakHash, allBlocks, findCollision2,
                 HashFunc, Collision(Collision))


data ExpMessagePart = ExpMessagePart {
    getAttempts :: Integer,
    getShortMessage :: B.ByteString,
    getLongMessage :: B.ByteString,
    getState :: B.ByteString
} deriving Show


k :: Integer
k = 16


stateSize :: Integer
hash :: HashFunc
(stateSize, hash) = makeWeakHash 3 0


main :: IO ()
main = do
    let blockSize' = fromIntegral blockSize

        targetMsg = BC.replicate (blockSize' * 2 ^ k) 'a'
        stateToBlockCount = Map.fromList $
            flip zip [0..] $
            scanl hash initState $
            chunksOf blockSize targetMsg

        finalExpMessageState = getState $ last expMessage

        bridgeLookup = flip Map.lookup stateToBlockCount .
            hash finalExpMessageState

        ((bridge, bridgeAttempts), Just blocksToDrop) = head $
            filter (isJust . snd) $
            map (fmap bridgeLookup) $
            zip (zip allBlocks [1..]) allBlocks

        blocksToExpand = blocksToDrop - k - 1
        bools = map (testBit blocksToExpand) [0..]
        chooseMessage bool = if bool then getLongMessage else getShortMessage

        targetEnd = B.drop (fromIntegral $ blockSize * blocksToDrop) targetMsg
        expStart = B.concat $ zipWith chooseMessage bools expMessage

        forgedMsg = B.concat [expStart, bridge, targetEnd]

        expAttempts = sum $ map getAttempts expMessage

    putStrLn $ "Target block count: " ++
        show (B.length targetMsg `div` blockSize')
    putStrLn $ "Target first block: " ++ show (B.take blockSize' targetMsg)
    putStrLn $ "Target hash: " ++ show (hash initState targetMsg)

    putStrLn $ "Expandable message block count: " ++ show (blocksToDrop - 1)

    putStrLn $ "Forged block count: " ++
        show (B.length forgedMsg `div` blockSize')
    putStrLn $ "Forged first block: " ++ show (B.take blockSize' forgedMsg)
    putStrLn $ "Forged hash: " ++ show (hash initState forgedMsg)

    putStrLn $ "Total compression function invocations: " ++
        show (expAttempts + bridgeAttempts + 2 ^ k)


initState :: B.ByteString
initState = B.replicate (fromIntegral stateSize) 0

expMessage :: [ExpMessagePart]
expMessage = take (fromIntegral k) $ getExpMessage 1 initState

getExpMessage :: Integer -> B.ByteString -> [ExpMessagePart]
getExpMessage dbCount state = part : getExpMessage (dbCount * 2) state'
    where
        dummyBlocks = BC.replicate (fromIntegral $ blockSize * dbCount) 'b'
        dummyState = hash state dummyBlocks

        Collision attempts shortMessage longBlock state' =
            findCollision2 hash state dummyState

        part = ExpMessagePart (attempts + dbCount) shortMessage
            (B.append dummyBlocks longBlock) state'
