import qualified Data.ByteString as B
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.String (fromString)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, ecbEncrypt, ecbDecrypt, BlockCipher)

import Data.Chunkable (chunksOf)
import Padding (pkcs7pad, pkcs7unpad)
import Util (buildQueryString, parseQueryString, randomlyKeyedCipherIO)


encrypt :: BlockCipher a => a -> B.ByteString -> B.ByteString
encrypt cipher email = ecbEncrypt cipher $ pad $ buildQueryString profile
    where
        pad = pkcs7pad $ fromIntegral $ blockSize cipher
        profile = Map.fromList [
                (fromString "email", email),
                (fromString "uid", fromString "10"),
                (fromString "role", fromString "user")
            ]


decrypt :: BlockCipher a =>
    a -> B.ByteString -> Maybe (Map.Map B.ByteString B.ByteString)
decrypt cipher = (>>= parseQueryString) . pkcs7unpad . ecbDecrypt cipher


aaa :: Integer -> B.ByteString
aaa n = B.replicate (fromIntegral n) $ fromIntegral $ ord 'A'


-- So, we've got an oracle function that will take a bytestring and
-- return "email=<our_string>&role=user&uid=10", escaped, padded,and
-- encrypted using AES in ECB mode.
--
-- The idea is to feed the oracle strings containing a bunch of 'A'
-- characters followed by "admin" padded to sixteen bytes. With the
-- right number of 'A's, we can get "admin" to line up with a block
-- boundary and obtain the encrypted block corresponding to "admin"
-- plus padding.
--
-- Once we have that, all we need to do is supply an input such that
-- the oracle-appended "user" lines up with a block boundary, then swap
-- the final block of the resulting output with our "admin" block.
--
-- We can also figure out that the oracle prepends 6 bytes to the input
-- by noting that the first output block doesn't change as long as the
-- first 10 bytes of input are the same. Its use of ECB mode, its block
-- size, and the fixed bytestring it appends to our input can then be
-- determined as in exercise 12. We can check for PKCS#7 by encrypting
-- our own block of 16 '\x10' characters.
--
-- One might object that our oracle function is unrealistic: since the
-- input is supposed to be an email address and the output a user
-- profile, it should append a different "uid" value for each input.
-- That's a good point, but it's also unlikely that a real-word
-- application would accept arbitrary bytes as a valid email. I think
-- the point of this one is just to illustrate the shortcomings of ECB.

main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128

    let encrypt' = encrypt cipher
        decrypt' = decrypt cipher
        adminAlignedInput = B.append (aaa 10) $ pkcs7pad 16 $ fromString "admin"
        adminBlock = (!! 1) $ chunksOf 16 $ encrypt' adminAlignedInput
        userBlocks = chunksOf 16 $ encrypt' $ aaa 4
        adminBlocks = init userBlocks ++ [adminBlock]

    print $ decrypt' $ B.concat adminBlocks

