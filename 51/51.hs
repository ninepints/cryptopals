import Control.Applicative (liftA2)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.List (groupBy, minimumBy, sortBy)
import System.Random (mkStdGen)

import Codec.Compression.Zlib (compress)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize)

import BlockCipher (cbcEncrypt, ctrCombine)
import ByteFormat (bytesToBase64)
import Padding (pkcs7pad)
import Util (randomBytes, randomBytesIO, randomlyKeyedCipherIO)


getCompressedSize :: B.ByteString -> (B.ByteString -> B.ByteString) ->
    B.ByteString -> Integer
getCompressedSize sessionId encrypt content = fromIntegral $ B.length request'
    where
        sessionIdEnc = bytesToBase64 sessionId
        lengthStr = BC.pack $ show $ B.length content
        request = BC.intercalate (BC.singleton '\n') $ [
                BC.pack "POST / HTTP/1.1",
                BC.pack "Host: hapless.com",
                B.append (BC.pack "Cookie: sessionid=") sessionIdEnc,
                B.append (BC.pack "Content-Length: ") lengthStr,
                B.empty,
                content
            ]
        request' = encrypt $ BL.toStrict $ compress $ BL.fromStrict $ request


main :: IO ()
main = do
    sessionId <- randomBytesIO 32
    putStrLn $ "Real session ID is " ++ show (bytesToBase64 sessionId)

    cipher <- randomlyKeyedCipherIO :: IO AES128
    let bs = fromIntegral $ blockSize cipher
    ctrIv <- randomBytesIO $ bs `div` 2
    cbcIv <- randomBytesIO $ bs

    let ctrOracle = getCompressedSize sessionId (ctrCombine cipher ctrIv)
        ctrId = guessSessionId (ctrOracle . prepSessionId) [B.empty]
    putStrLn $ "Recovered session ID from CTR: " ++ show (bytesToBase64 ctrId)

    let cbcPadEncrypt = cbcEncrypt cipher cbcIv . pkcs7pad bs
        cbcOracle = getCompressedSize sessionId cbcPadEncrypt
        cbcId = guessSessionId (wrapOracle cbcOracle) [B.empty]
    putStrLn $ "Recovered session ID from CBC: " ++ show (bytesToBase64 cbcId)


prepSessionId :: B.ByteString -> B.ByteString
prepSessionId = B.append (BC.pack "sessionid=") . bytesToBase64


guessSessionId :: (B.ByteString -> Integer) -> [B.ByteString] -> B.ByteString
guessSessionId oracle bytesSoFar = if B.length (head bytesSoFar) == 32
    then minimumBy (compare `on` oracle') bytesSoFar
    else guessSessionId oracle bestBytes
    where
        oracle' = oracle
        newBytes = [B.snoc bytes byte | bytes <- bytesSoFar, byte <- [0..255]]
        bytesAndTimes = map (liftA2 (,) id oracle') newBytes
        bestBytes = map fst $
                    head $
                    groupBy ((==) `on` snd) $
                    sortBy (compare `on` snd) bytesAndTimes


-- When we're dealing with CBC encryption, we only notice compression
-- improvements when they cross a block boundary (i.e. we remove 16
-- bytes under AES128). We can't guess 16 bytes of the session key at
-- once, but for each single-byte guess, we can append varying amounts
-- of uncompressable data to the end of the request and get a better
-- handle on how compressed things are.
wrapOracle :: (B.ByteString -> Integer) -> B.ByteString -> Integer
wrapOracle oracle content = origLength + 1 - bytesToOverflow
    where
        content' = prepSessionId content
        origLength = oracle content'
        bytesToOverflow = head $ filter causesOverflow [1..]
        causesOverflow nBytes = origLength < newLength
            where
                newLength = oracle (B.append content' uncompressable)
                (uncompressable, _) = randomBytes nBytes $ mkStdGen 0
