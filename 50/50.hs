import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, cipherInit, BlockCipher)
import Crypto.Error (CryptoFailable(..))

import BlockCipher (cbcEncrypt)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad, constantPad)
import Util (xorBytes)


message1 :: B.ByteString
message1 = BC.pack "alert('MZA who was that?');\n"


message2 :: B.ByteString
message2 = BC.pack "alert('Ayo, the Wu is back!');\n"


getMac :: BlockCipher a =>
    Bool -> a -> B.ByteString -> B.ByteString -> B.ByteString
getMac pad cipher iv plaintext = last chunks
    where
        bs = fromIntegral $ blockSize cipher
        plaintext' = if pad then pkcs7pad bs plaintext else plaintext
        chunks = chunksOf bs $ cbcEncrypt cipher iv plaintext'


main :: IO ()
main = let
    cipher :: AES128
    CryptoPassed cipher = cipherInit $ BC.pack "YELLOW SUBMARINE"
    bs = blockSize cipher
    iv = B.replicate bs 0

    macWithPad = getMac True cipher iv
    macSansPad = getMac False cipher iv

    slash = fromIntegral $ ord '/'
    message2' = constantPad (fromIntegral bs) slash message2
    message2'' = B.append message2' $ B.replicate bs slash
    mac = macSansPad message2''
    newFirstBlock = xorBytes mac $ B.take bs message1
    message1' = B.append newFirstBlock $ B.drop bs message1
    combinedMessage = B.append message2'' message1'

    in do
        putStrLn $ show message1
        putStrLn $ show $ macWithPad message1
        putStrLn $ show combinedMessage
        putStrLn $ show $ macWithPad combinedMessage

        B.writeFile "demo.html" $ B.concat [
                BC.pack "<script>\n",
                combinedMessage,
                BC.pack "</script>"
            ]
