import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)

import Snap.Core (getParam, ifTop, modifyResponse,
                  setResponseCode, writeBS, Snap)
import Snap.Http.Server (quickHttpServe)

import ByteFormat (hexToBytes)
import Util (randomBytesIO)


main :: IO ()
main = do
    [delayMs] <- map read <$> getArgs
    when (delayMs < 0) $ error "Negative delay"
    secretHash <- randomBytesIO 20
    quickHttpServe $ ifTop $ handler secretHash delayMs


handler :: B.ByteString -> Int -> Snap ()
handler secretHash delayMs = do
    encodedHash <- getParam $ B.pack "hash"
    case encodedHash >>= hexToBytes of
        Just hash | B.length hash == 20 -> do
            hashesEqual <- liftIO $ slowCompare delayMs hash secretHash
            if hashesEqual
                then writeBS $ B.pack "Ok!"
                else do
                    modifyResponse $ setResponseCode 400
                    writeBS $ B.pack "No good!"
        _ -> do
            modifyResponse $ setResponseCode 400
            writeBS $ B.pack "That wasn't even a valid hash"


slowCompare :: Int -> B.ByteString -> B.ByteString -> IO Bool
slowCompare delayMs xs ys = slowCompare' $ B.zip xs ys
    where
        slowCompare' [] = return True
        slowCompare' ((x, y):rest) = do
            threadDelay $ 1000 * delayMs
            if x /= y then return False else slowCompare' rest
