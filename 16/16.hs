import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, BlockCipher)

import BlockCipher (cbcEncrypt, cbcDecrypt)
import ByteFormat (urlEscape)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad, pkcs7unpad)
import Util (randomBytesIO, randomlyKeyedCipherIO, xorBytes)


encrypt :: BlockCipher a => a -> B.ByteString -> B.ByteString -> B.ByteString
encrypt cipher iv input = cbcEncrypt cipher iv $ pad plaintext
    where
        pad = pkcs7pad $ fromIntegral $ blockSize cipher
        plaintext = B.concat [
                B.pack "comment1=cooking MCs;userdata=",
                urlEscape input,
                B.pack ";comment2= like a pound of bacon"
            ]


decrypt :: BlockCipher a => a -> B.ByteString -> B.ByteString -> Bool
decrypt cipher iv = checkForAdmin . pkcs7unpad . cbcDecrypt cipher iv
    where
        checkForAdmin = fromMaybe False . fmap containsAdmin
        containsAdmin = B.isInfixOf $ B.pack ";admin=true;"


aaa :: Integer -> B.ByteString
aaa n = B.replicate (fromIntegral n) 'A'


main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128
    iv <- randomBytesIO $ fromIntegral $ blockSize cipher

    let blocks = chunksOf 16 $ encrypt cipher iv $ aaa 48
        mask = xorBytes (aaa 16) $ B.pack ";admin=true;b=b;"
        blocks' = take 2 blocks ++ xorBytes (blocks !! 2) mask : drop 3 blocks
        result = decrypt cipher iv $ B.concat blocks'

    putStrLn $ "Did it work? " ++ show result
