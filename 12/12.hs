import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, ecbEncrypt, BlockCipher)

import ByteFormat (base64ToBytes)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad)
import Util (randomlyKeyedCipherIO, uniqueness)


secret :: BS.ByteString
Just secret = base64ToBytes $ BC.pack $ "Um9s" ++
    "bGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFp" ++
    "ciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0" ++
    "IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"


type Oracle = BS.ByteString -> BS.ByteString

-- | Create an oracle function that appends a secret to the input,
-- pads the result, then encrypts it all in ECB mode.
makeOracle :: BlockCipher c => c -> Oracle
makeOracle cipher input = ecbEncrypt cipher $ pad $ BS.append input secret
    where pad = pkcs7pad $ fromIntegral $ blockSize cipher


aaa :: Integer -> BS.ByteString
aaa n = BC.replicate (fromIntegral n) 'A'


findBlockSize :: Oracle -> Integer
findBlockSize oracle = firstSizeYieldingStaticFirstBlock [1..]
    where
        firstSizeYieldingStaticFirstBlock (i:j:k:ks)
            | x == y && y == z = i
            | otherwise = firstSizeYieldingStaticFirstBlock (j:k:ks)
            where
                x = BS.take (fromIntegral i) $ oracle $ aaa i
                y = BS.take (fromIntegral i) $ oracle $ aaa j
                z = BS.take (fromIntegral i) $ oracle $ aaa k


findEcb :: Oracle -> Integer -> Bool
findEcb oracle blockLength = uniqueness (chunksOf blockLength output) < 1
    where
        input = aaa $ 3 * blockLength
        output = oracle input


findSecret :: Oracle -> Integer -> BS.ByteString
findSecret oracle blockLength = findSecret' oracle blockLength BS.empty

findSecret' :: Oracle -> Integer -> BS.ByteString -> BS.ByteString
findSecret' oracle blockLength bytesSoFar
    | done = bytesSoFar
    | otherwise = findSecret' oracle blockLength $ BS.snoc bytesSoFar nextByte
    where
        -- Check if we're done by passing bytesSoFar to the oracle
        doneInput = pkcs7pad blockLength bytesSoFar
        doneOutput = BS.take (BS.length doneInput) $ oracle doneInput
        done = doneOutput == oracle BS.empty

        -- Build 256 inputs we'll pass to the oracle to identify the next byte
        numBytesSoFar = fromIntegral $ BS.length bytesSoFar
        prefix = aaa $ blockLength - numBytesSoFar `mod` blockLength - 1
        inputs = map (BS.snoc (BS.append prefix bytesSoFar)) [0..255]

        -- Function truncating oracle output to just the blocks of interest
        truncate = BS.take $ BS.length $ head inputs

        getOutput = truncate . oracle
        outputMap = Map.fromList $ zip (map getOutput inputs) [0..255]
        referenceOutput = getOutput prefix
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







