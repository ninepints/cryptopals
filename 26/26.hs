import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, BlockCipher)

import BlockCipher (ctrCombine)
import ByteFormat (urlEscape)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad, pkcs7unpad)
import Util (randomBytesIO, randomlyKeyedCipherIO, xorBytes)


-- This is exactly the same as exercise 16, but with ctrCombine
-- substituted for ecbEncrypt/ecbDecrypt and a slightly shorter
-- string of 'A's in the chosen plaintext. Previously, we added
-- two blocks of 'A's just so we wouldn't scramble a potentially
-- "important" CBC block when masking the following block. Now we
-- can just mask the single block and be done with it.


encrypt :: BlockCipher a => a -> B.ByteString -> B.ByteString -> B.ByteString
encrypt cipher iv input = ctrCombine cipher iv $ pad plaintext
    where
        pad = pkcs7pad $ fromIntegral $ blockSize cipher
        plaintext = B.concat [
                B.pack "comment1=cooking MCs;userdata=",
                urlEscape input,
                B.pack ";comment2= like a pound of bacon"
            ]


decrypt :: BlockCipher a => a -> B.ByteString -> B.ByteString -> Bool
decrypt cipher iv = checkForAdmin . pkcs7unpad . ctrCombine cipher iv
    where
        checkForAdmin = fromMaybe False . fmap containsAdmin
        containsAdmin = B.isInfixOf $ B.pack ";admin=true;"


aaa :: Integer -> B.ByteString
aaa n = B.replicate (fromIntegral n) 'A'


main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128
    iv <- randomBytesIO $ fromIntegral $ blockSize cipher `div` 2

    let blocks = chunksOf 16 $ encrypt cipher iv $ aaa 32
        mask = xorBytes (aaa 16) $ B.pack ";admin=true;b=b;"
        blocks' = take 2 blocks ++ xorBytes (blocks !! 2) mask : drop 3 blocks
        result = decrypt cipher iv $ B.concat blocks'

    putStrLn $ "Did it work? " ++ show result
