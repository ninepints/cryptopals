import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Tuple (uncurry)
import System.Random (randomRIO)
import Text.Printf (printf)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, ecbEncrypt, BlockCipher)

import ByteFormat (base64ToBytes)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad)
import Util (mode, randomBytesIO, randomlyKeyedCipherIO)


secret :: BS.ByteString
Just secret = base64ToBytes $ BC.pack $ "Um9s" ++
    "bGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFp" ++
    "ciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0" ++
    "IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"


type Oracle = BS.ByteString -> BS.ByteString

-- | Create an oracle function that prepends a prefix/appends a secret
-- to the input, pads the result, then encrypts it all in ECB mode.
makeOracle :: BlockCipher c => c -> BS.ByteString -> Oracle
makeOracle cipher prefix input = ecbEncrypt cipher $ pad plaintext
    where
        pad = pkcs7pad $ fromIntegral $ blockSize cipher
        plaintext = BS.concat [prefix, input, secret]


aaa :: Integer -> BS.ByteString
aaa n =  BC.replicate (fromIntegral n) 'A'

bbb :: Integer -> BS.ByteString
bbb n =  BC.replicate (fromIntegral n) 'B'

ccc :: Integer -> BS.ByteString
ccc n =  BC.replicate (fromIntegral n) 'C'


-- We're going to find the prefix length by encrypting 16 'A's down to
-- 1 and looking at how many initial blocks the ciphertexts share. This
-- will break if the secret starts with an 'A', so we'll repeat the
-- exercise with different characters afterwards.
--
-- prefix length == 0 -> no shared blocks between 16 and 15 'A's
-- prefix length mod 16 == 1 -> shared block count changes at 14 'A's
-- prefix length mod 16 == 2 -> shared block count changes at 13 'A's
-- ...
-- prefix length mod 16 == 15 -> shared block count changes at 0 'A's
-- otherwise the prefix length is a multiple of 16 greater than zero

findPrefixLength :: Oracle -> (Integer -> BS.ByteString) -> Integer
findPrefixLength oracle fill = completeBlockLength + additionalLength
    where
        completeBlockLength = 16 * (shared16To15 - 1)
        additionalLength = findSharedBlockShift [15,14..0]

        shared16To15 = sharedBlocks (oracle $ fill 16) (oracle $ fill 15)

        findSharedBlockShift (x:[]) = 16 - x
        findSharedBlockShift (x:y:xs) = if sharedXToY < shared16To15
            then 16 - x
            else findSharedBlockShift (y:xs)
            where sharedXToY = sharedBlocks (oracle $ fill x) (oracle $ fill y)

        sharedBlocks a b = fromIntegral $ length $ takeWhile eq $ pairs
            where
                eq = uncurry (==)
                pairs = zip (chunksOf 16 a) (chunksOf 16 b)


findSecretLength :: Oracle -> Integer -> Integer
findSecretLength oracle prefixLength = totalLength - prefixLength
    where
        integerLength = fromIntegral . length
        emptyBlockCount = integerLength $ chunksOf 16 $ oracle BS.empty

        findSizeAddingBlock (x:xs) = if blockCount > emptyBlockCount
            then x
            else findSizeAddingBlock xs
            where blockCount = integerLength $ chunksOf 16 $ oracle $ aaa x
        firstSizeAddingBlock = findSizeAddingBlock [1..]

        totalLength = if firstSizeAddingBlock == 16
            then 16 * (emptyBlockCount - 1)  -- Last block is all padding
            else 16 * emptyBlockCount - firstSizeAddingBlock


-- Once we know the prefix length, finding the secret is similar to
-- exercise 12.

findSecret :: Oracle -> BS.ByteString
findSecret oracle = findSecret' oracle prefixLength secretLength BS.empty
    where
        prefixLength = mode $ map (findPrefixLength oracle) [aaa, bbb, ccc]
        secretLength = findSecretLength oracle prefixLength

findSecret' :: Oracle -> Integer -> Integer -> BS.ByteString -> BS.ByteString
findSecret' oracle prefixLength secretLength bytesSoFar
    | fromIntegral (BS.length bytesSoFar) == secretLength = bytesSoFar
    | otherwise = findSecret' oracle prefixLength secretLength bytesSoFar'
    where
        -- Build 256 inputs we'll pass to the oracle to identify the next byte
        numBytesSoFar = prefixLength + (fromIntegral $ BS.length bytesSoFar)
        ourPrefix = aaa $ 16 - numBytesSoFar `mod` 16 - 1
        inputs = map (BS.snoc (BS.append ourPrefix bytesSoFar)) [0..255]

        -- Function truncating oracle output to just the blocks of interest
        addPrefixLength = (+) $ fromIntegral prefixLength
        truncate = BS.take $ addPrefixLength $ BS.length $ head inputs

        getOutput = truncate . oracle
        outputMap = Map.fromList $ zip (map getOutput inputs) [0..255]
        referenceOutput = getOutput ourPrefix
        nextByte = outputMap Map.! referenceOutput
        bytesSoFar' = BS.snoc bytesSoFar nextByte


main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128
    prefixLength <- randomRIO (1, 16)
    prefix <- randomBytesIO prefixLength

    let oracle = makeOracle cipher prefix

    putStrLn $ printf "The secret is %v" $ show $ findSecret oracle
