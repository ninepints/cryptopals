import Control.Applicative (liftA2)
import qualified Data.ByteString as B
import Data.Function (on)
import Data.List (groupBy, intercalate)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, cipherKeySize, cipherInit, ecbEncrypt,
                            KeySizeSpecifier(..))
import Crypto.Error (CryptoFailable(..))

import ByteFormat (integerToBytes)
import Padding (constantPadLeft)
import Util (powerList, pairs)


type MDHash = B.ByteString -> B.ByteString -> B.ByteString

data Collision a = Collision {
    getAttempts :: a,
    getInput1 :: B.ByteString,
    getInput2 :: B.ByteString
} deriving Show

type CollidingBlock = Collision Integer
type CollidingPair = Collision (Integer, Integer)

bs :: Integer
bs = fromIntegral $ blockSize (undefined :: AES128)


makeHash :: Integer -> Integer -> (Integer, MDHash)
makeHash size offset = (size, f)
    where
        size' = fromIntegral size
        offset' = fromIntegral offset
        f state bytes
            | B.length state /= size' = error "Bad state size"
            | B.length bytes `mod` bs' /= 0 = error "Bad input size"
            | otherwise = B.take size' $ B.drop offset' cipherOutput
            where
                cipher :: AES128
                KeySizeFixed ks = cipherKeySize cipher
                ks' = fromIntegral ks
                bs' = fromIntegral bs
                CryptoPassed cipher = cipherInit $ constantPadLeft ks' 0 state
                cipherOutput = ecbEncrypt cipher bytes


main :: IO ()
main = do
    let f = makeHash 1 0
        g = makeHash 2 1

        fBlocks = getCollidingBlocks f
        gBlocks = getCollidingBlocks g
        hPairs = getCollidingPairs f g

        -- How many consecutive colliding blocks do we need in
        -- order to generate some number of colliding pairs?
        pairsFromBlocks 1 = 1
        pairsFromBlocks b = sum [1..(2^b)-1] + pairsFromBlocks (b-1)
        blocksReqForPairs p = head $ filter ((>= p) . pairsFromBlocks) [1..]

        nthElem l = (!!) l . flip (-) 1
        ns = [100, 1000, 10000, 100000]

        fCounts = map (getAttempts . nthElem fBlocks . blocksReqForPairs) ns
        gCounts = map (getAttempts . nthElem gBlocks . blocksReqForPairs) ns
        hCounts = map (getAttempts . nthElem hPairs) ns

    putStrLn "Calls to f required for 100/1k/10k/100k pairs:"
    putStrLn $ intercalate "\n" $ map show fCounts
    putStrLn "Calls to g required for 100/1k/10k/100k pairs:"
    putStrLn $ intercalate "\n" $ map show gCounts
    putStrLn "Calls to (f,g) required for 100/1k/10k/100k pairs in f || g:"
    putStrLn $ intercalate "\n" $ map show hCounts


getCollidingBlocks :: (Integer, MDHash) -> [CollidingBlock]
getCollidingBlocks (fs, f) = _getCollidingBlocks f initState 0
    where initState = B.replicate (fromIntegral fs) 0


getCollidingPairs :: (Integer, MDHash) -> (Integer, MDHash) -> [CollidingPair]
getCollidingPairs (fs, f) (gs, g) = result
    where
        fInitState = B.replicate (fromIntegral fs) 0
        gInitState = B.replicate (fromIntegral gs) 0

        fBlocks = _getCollidingBlocks f fInitState 0

        -- Groups of colliding bytestrings under f
        -- The first value of each tuple is equal across each group and
        -- is the number of f invocations it took to find that group
        fBlocksGrouped :: [[(Integer, B.ByteString)]]
        fBlocksGrouped = groupBy ((==) `on` fst) $ do
            i <- [0..]
            bools <- powerList [False, True] (fromIntegral i + 1)
            let collisionsThroughI = take (i+1) fBlocks
                chooseBlock bool = if bool then getInput2 else getInput1
                blocks = zipWith chooseBlock bools collisionsThroughI
            return (getAttempts $ fBlocks !! i, B.concat blocks)

        -- Pairs of colliding bytestrings under f
        fBlocksPaired = fBlocksGrouped >>= pairs

        collidesUnderG = (==) `on` g gInitState

        zipPairs ((fAttempts, input1), (fAttempts', input2)) i
            | fAttempts /= fAttempts' = error "Your code is off somewhere"
            | otherwise = Collision (fAttempts, i) input1 input2

        result = filter (liftA2 collidesUnderG getInput1 getInput2) $
            zipWith zipPairs fBlocksPaired [1..]


_getCollidingBlocks :: MDHash -> B.ByteString -> Integer -> [CollidingBlock]
_getCollidingBlocks f state nAttempts = collision : collisions
    where
        prepInteger = constantPadLeft bs 0 . integerToBytes
        getNextState = f state . prepInteger
        state' = getNextState 0
        possibleMatches = [(i, getNextState i) | i <- [1..]]
        filteredMatches = filter ((==) state' . snd) possibleMatches
        (addlAttempts, _) = head filteredMatches
        nAttempts' = nAttempts + 1 + addlAttempts
        collision = Collision nAttempts'
            (prepInteger 0) (prepInteger addlAttempts)
        collisions = _getCollidingBlocks f state' nAttempts'
