import qualified Data.ByteString as B

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, cipherKeySize, cipherInit,
                            BlockCipher, KeySizeSpecifier(..))
import Crypto.Error (CryptoFailable(..))

import BlockCipher (cbcEncrypt, cbcDecrypt)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad, pkcs7unpad)
import Util (randomBytesIO, xorBytes)


encrypt :: BlockCipher a => a -> B.ByteString -> B.ByteString -> B.ByteString
encrypt cipher iv input = cbcEncrypt cipher iv $ pad input
    where pad = pkcs7pad $ fromIntegral $ blockSize cipher


decrypt :: BlockCipher a =>
    a -> B.ByteString -> B.ByteString -> Either B.ByteString ()
decrypt cipher iv input = case maybeUnpadded of
    Nothing -> Left padded
    Just unpadded -> if B.any (>127) unpadded
        then Left unpadded
        else Right ()
    where
        padded = cbcDecrypt cipher iv input
        maybeUnpadded = pkcs7unpad padded


main :: IO ()
main = do
    let _cipher :: AES128  -- We don't care about this value, only its type
        _cipher = undefined
        KeySizeFixed keyLength = cipherKeySize _cipher
    key <- randomBytesIO $ fromIntegral keyLength

    let CryptoPassed cipher = cipherInit key :: CryptoFailable AES128
        encrypt' = encrypt cipher key
        decrypt' = decrypt cipher key

    plaintext <- randomBytesIO 48

    let bs = blockSize cipher
        blocks = chunksOf (fromIntegral bs) $ encrypt' plaintext
        blocks' = head blocks : B.replicate bs 0 : head blocks : drop 3 blocks
        Left result = decrypt' $ B.concat blocks'

        resultBlocks = chunksOf (fromIntegral bs) result
        key' = xorBytes (resultBlocks !! 0) (resultBlocks !! 2)

    putStrLn $ "Actual key: " ++ show key
    putStrLn $ "Recovered key: " ++ show key'
