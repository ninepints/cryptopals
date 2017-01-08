import qualified Data.ByteString as B
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, ecbEncrypt, BlockCipher)

import qualified ByteFormat
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad)
import Util (randomlyKeyedCipherIO, uniqueness)


secret :: B.ByteString
Just secret = ByteFormat.b64ToBytes $ "Um9s" ++
    "bGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFp" ++
    "ciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0" ++
    "IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"


type Oracle = B.ByteString -> B.ByteString

-- | Create an oracle function that appends a secret to the input,
-- pads the result, then encrypts it all in ECB mode.
makeOracle :: BlockCipher c => c -> Oracle
makeOracle cipher input = ecbEncrypt cipher $ pad $ B.append input secret
    where pad = pkcs7pad $ fromIntegral $ blockSize cipher


aaa :: Integer -> B.ByteString
aaa n = B.pack $ map (fromIntegral . ord) $ replicate (fromIntegral n) 'A'


findBlockSize :: Oracle -> Integer
findBlockSize oracle = firstSizeYieldingStaticFirstBlock [1..]
    where
        firstSizeYieldingStaticFirstBlock (i:j:k:ks)
            | x == y && y == z = i
            | otherwise = firstSizeYieldingStaticFirstBlock (j:k:ks)
            where
                x = B.take (fromIntegral i) $ oracle $ aaa i
                y = B.take (fromIntegral i) $ oracle $ aaa j
                z = B.take (fromIntegral i) $ oracle $ aaa k


findEcb :: Oracle -> Integer -> Bool
findEcb oracle blockLength = uniqueness (chunksOf blockLength output) < 1
    where
        input = aaa $ 3 * blockLength
        output = oracle input


findSecret :: Oracle -> Integer -> B.ByteString
findSecret oracle blockLength = findSecret' oracle blockLength B.empty

findSecret' :: Oracle -> Integer -> B.ByteString -> B.ByteString
findSecret' oracle blockLength bytesSoFar
    | done = bytesSoFar
    | otherwise = findSecret' oracle blockLength $ B.snoc bytesSoFar nextByte
    where
        -- Check if we're done by passing bytesSoFar to the oracle
        doneInput = pkcs7pad blockLength bytesSoFar
        doneOutput = B.take (B.length doneInput) $ oracle doneInput
        done = doneOutput == oracle B.empty

        -- Build 256 inputs we'll pass to the oracle to identify the next byte
        numBytesSoFar = fromIntegral $ B.length bytesSoFar
        prefix = aaa $ blockLength - numBytesSoFar `mod` blockLength - 1
        inputs = map (B.snoc (B.append prefix bytesSoFar)) [0..255]

        -- Function truncating oracle output to just the blocks of interest
        truncate = B.take $ B.length $ head inputs

        outputMap = Map.fromList $ zip (map (truncate . oracle) inputs) [0..255]
        referenceOutput = truncate $ oracle prefix
        nextByte = outputMap Map.! referenceOutput


main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128

    let oracle = makeOracle cipher
        blockLength = findBlockSize oracle
        isEcb = findEcb oracle blockLength

    putStrLn $ printf "Block size is %u" blockLength
    putStrLn $ printf "Using ECB mode? %v" $ show isEcb
    putStrLn $ printf "The secret is %v" $ show $ findSecret oracle blockLength







