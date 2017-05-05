import Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Function (on)
import Data.List (maximumBy, sortBy)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Map.Lazy as Map
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Printf (printf)

import Crypto.Hash.Algorithms (SHA1(..))
import Crypto.Hash.IO (hashDigestSize)
import Network.Http.Client (get, getStatusCode, Response)
import System.IO.Streams.Internal (InputStream)

import ByteFormat (bytesToHex)
import Padding (constantPad)


main :: IO ()
main = do
    [requestsPerGuess] <- map read <$> getArgs
    when (requestsPerGuess < 1) $ error "Request count zero or negative"
    guessAllBytes requestsPerGuess B.empty


hmacSize :: Int
hmacSize = hashDigestSize SHA1


guessAllBytes :: Integer -> B.ByteString -> IO ()
guessAllBytes requestsPerGuess bytesSoFar = unless done $ do
    bytesSoFar' <- guessNextByte requestsPerGuess bytesSoFar
    guessAllBytes requestsPerGuess bytesSoFar'
    where done = B.length bytesSoFar >= hmacSize


guessNextByte :: Integer -> B.ByteString -> IO B.ByteString
guessNextByte requestsPerGuess bytesSoFar = do
    putStrLn $ printf "Bytes so far: " ++ show (bytesToHex bytesSoFar)

    let guesses = replicate (fromIntegral requestsPerGuess) [0..255] >>= id
        actions = map (timeRequest . B.snoc bytesSoFar) guesses

        addProgress = zipWith addProgress' [1..]
            where addProgress' i = (>>) $ when (i `mod` 256 == 0) $ putStr "."

    -- NB: This makes our request actions lazy. Instead of sending all
    -- the requests with the next line, we'll send requests when the
    -- results are needed. We save several requests if the server
    -- returns a 200, since the remaining results are discarded.
    --
    -- Side effects are still contained by the IO action
    -- returned by guessNextByte.
    results <- sequence $ map unsafeInterleaveIO $ addProgress actions

    let is2xx = (== 2) . (`div` 100) . fst
        entry2xx = is2xx . snd
        timeCmp = comparing snd
        entryTimeCmp = timeCmp `on` snd
        medianBy cmp xs = sortBy cmp xs !! (length xs `div` 2)

        maybe2xx = fst <$> listToMaybe (filter entry2xx $ zip guesses results)

        wrappedResults = map (:[]) results
        resultsByGuess = Map.fromListWith (++) $ zip guesses wrappedResults
        fastestByGuess = fmap (medianBy timeCmp) resultsByGuess
        slowestByte = fst $ maximumBy entryTimeCmp $ Map.toList fastestByGuess

        nextByte = fromMaybe slowestByte maybe2xx
        bytesSoFar' = B.snoc bytesSoFar nextByte

    if isJust maybe2xx
        then putStrLn $ "Got HTTP 200 with " ++ show (bytesToHex bytesSoFar')
        else putStrLn ""
    return bytesSoFar'


timeRequest :: B.ByteString -> IO (Integer, Integer)
timeRequest hmacPrefix = do
    let pad = constantPad (fromIntegral hmacSize) 0
        hmacFormatted = bytesToHex $ pad hmacPrefix
        urlPrefix = BC.pack "http://127.0.0.1:8000/?message=hello!&hmac="
        url = B.append urlPrefix hmacFormatted
    start <- getPOSIXTime
    statusCode <- get url returnStatusCode
    end <- getPOSIXTime
    return (statusCode, round $ (end - start) * 1000000)


returnStatusCode :: Response -> InputStream B.ByteString -> IO Integer
returnStatusCode response _ = return $ fromIntegral $ getStatusCode response
