{-# LANGUAGE BangPatterns #-}

import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import System.Random (split, newStdGen, randoms, RandomGen)

import qualified Crypto.Cipher.RC4 as RC4

import ByteFormat (base64ToBytes)
import Data.Chunkable (chunksOf)


secret :: B.ByteString
Just secret = base64ToBytes $
    BC.pack "QkUgU1VSRSBUTyBEUklOSyBZT1VSIE9WQUxUSU5F"


encrypt :: B.ByteString -> B.ByteString -> B.ByteString
encrypt key bytes = snd $ RC4.combine (RC4.initialize key) bytes


encryptRequest :: B.ByteString -> B.ByteString -> B.ByteString
encryptRequest key path = encrypt key request
    where request = B.concat [
                BC.pack "GET ", path, BC.singleton '\n',
                BC.pack "Set-Cookie: sessionId=", secret, BC.pack ";\n"
            ]


main :: IO ()
main = do
    (g1, g2) <- split <$> newStdGen
    let dummyKeys = randomKeys g1
        dummyPT = B.replicate 64 0
        dummyCTs = map (flip encrypt dummyPT) dummyKeys
        (keyByte48, keyByte64) = getTopBytes dummyCTs
    putStrLn $ "Top 48th key byte is " ++ show keyByte48
    putStrLn $ "Top 64th key byte is " ++ show keyByte64

    let minReqSize = B.length $ encryptRequest (head $ randomKeys g2) $
            BC.singleton '/'
        startPath = BC.cons '/' $ BC.replicate (64 - minReqSize) 'a'

    guessedBytes <- guessBytes keyByte48 keyByte64 B.empty B.empty startPath
    putStrLn $ "Guessed bytes " ++ show guessedBytes


guessBytes :: Word8 -> Word8 -> B.ByteString ->
    B.ByteString -> B.ByteString -> IO B.ByteString
guessBytes keyByte48 keyByte64 bytesFrom48 bytesFrom64 path
    | done = return allBytes
    | otherwise = recurse
    where
        done = B.length bytesFrom48 >= 16 && BC.elem '=' bytesFrom48
        allBytes = B.append (B.take 16 bytesFrom48) bytesFrom64

        recurse = do
            putStrLn $ "Requesting " ++ BC.unpack path
            gen <- newStdGen
            let cts = map (flip encryptRequest path) $ randomKeys gen
                (encByte48, encByte64) = getTopBytes cts
                bytesFrom48' = B.cons (encByte48 `xor` keyByte48) bytesFrom48
                bytesFrom64' = B.cons (encByte64 `xor` keyByte64) bytesFrom64
                path' = BC.snoc path 'a'

            guessBytes keyByte48 keyByte64 bytesFrom48' bytesFrom64' path'


randomKeys :: RandomGen g => g -> [B.ByteString]
randomKeys = map B.pack . chunksOf keyLength . take byteCount . randoms
    where
        sampleCount = 2 ^ 26
        keyLength = 16
        byteCount = fromIntegral $ keyLength * sampleCount


getTopBytes :: [B.ByteString] -> (Word8, Word8)
getTopBytes = getTopBytes' Map.empty Map.empty
    where
        getTopBytes' counter48 counter64 [] = (topByte48, topByte64)
            where
                mostFreq = fst . maximumBy (compare `on` snd) . Map.toList
                topByte48 = mostFreq counter48
                topByte64 = mostFreq counter64

        getTopBytes' !counter48 !counter64 (ct:cts) =
            getTopBytes' counter48' counter64' cts
            where
                counter48' = Map.insertWith (+) (B.index ct 47) 1 counter48
                counter64' = Map.insertWith (+) (B.index ct 63) 1 counter64
