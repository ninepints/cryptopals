import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)

import Crypto.Hash.Algorithms (SHA1(..))
import Crypto.Hash.IO (hashDigestSize)
import Snap.Core (getParam, ifTop, modifyResponse,
                  setResponseCode, writeBS, Snap)
import Snap.Http.Server (quickHttpServe)

import ByteFormat (hexToBytes)
import qualified HMAC
import Util (randomBytesIO)


main :: IO ()
main = do
    [delayMs] <- map read <$> getArgs
    when (delayMs < 0) $ error "Negative delay"
    key <- randomBytesIO 16
    quickHttpServe $ ifTop $ handler key delayMs


handler :: B.ByteString -> Int -> Snap ()
handler key delayMs = do
    maybeMessage <- getParam $ B.pack "message"
    maybeHmac <- getParam $ B.pack "hmac"
    case (maybeMessage, maybeHmac >>= hexToBytes) of
        (Just message, Just hmac) | B.length hmac == hashDigestSize SHA1 -> do
            let correctHmac = HMAC.hmac SHA1 key message
            hmacsMatch <- liftIO $ slowCompare delayMs hmac correctHmac
            if hmacsMatch
                then writeBS $ B.pack "Ok"
                else do
                    modifyResponse $ setResponseCode 400
                    writeBS $ B.pack "No good"
        _ -> do
            modifyResponse $ setResponseCode 400
            writeBS $ B.pack "Really no good"


slowCompare :: Int -> B.ByteString -> B.ByteString -> IO Bool
slowCompare delayMs xs ys = slowCompare' $ B.zip xs ys
    where
        slowCompare' [] = return True
        slowCompare' ((x, y):rest) = do
            threadDelay $ 1000 * delayMs
            if x /= y then return False else slowCompare' rest
