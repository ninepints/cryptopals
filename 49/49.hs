import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, BlockCipher)

import BlockCipher (cbcEncrypt)
import Data.Chunkable (chunksOf)
import Padding (pkcs7pad)
import Util (randomBytesIO, randomlyKeyedCipherIO, xorBytes)


getMac :: BlockCipher a => a -> B.ByteString -> B.ByteString -> B.ByteString
getMac cipher iv plaintext = last chunks
    where
        bs = fromIntegral $ blockSize cipher
        chunks = chunksOf bs $ cbcEncrypt cipher iv $ pkcs7pad bs plaintext


checkMac :: BlockCipher a =>
    a -> B.ByteString -> B.ByteString -> B.ByteString -> Bool
checkMac = (fmap . fmap . fmap) (==) getMac


main :: IO ()
main = demo1 >> demo2


demo1 :: IO ()
demo1 = do
    putStrLn "Demo 1"

    cipher <- randomlyKeyedCipherIO :: IO AES128
    iv <- randomBytesIO $ fromIntegral $ blockSize cipher

    let message = BC.pack "from=mallory&to=mallory_alt&amount=1000000"
        altMessage = BC.pack "from=alice&a&to=mallory_alt&amount=1000000"
        mac = getMac cipher iv message
        mask = B.take (blockSize cipher) $ xorBytes message altMessage
        altIv = xorBytes iv mask

    putStrLn $ "Legit message valid? " ++
        (show $ checkMac cipher iv message mac)
    putStrLn $ "Altered message valid? " ++
        (show $ checkMac cipher altIv altMessage mac)


demo2 :: IO ()
demo2 = do
    putStrLn "Demo 2"

    cipher <- randomlyKeyedCipherIO :: IO AES128

    let iv = B.replicate (blockSize cipher) 0
        message = BC.pack "from=alice&tx_list=bob:50;chuck:25"
        mac = getMac cipher iv message

        -- Our (adjusted) message extension shares a MAC with the
        -- original message plus extension. This attack requires the
        -- client to generate a MAC for our extension, which seems a
        -- bit impractical - if you can already get MACs for arbitrary
        -- messages, it's not much of an attack, is it?
        extension = BC.pack ";mallory:1000000"
        extMasked = xorBytes mac extension
        extMac = getMac cipher iv extMasked
        pad = pkcs7pad $ fromIntegral $ blockSize cipher
        extMessage = B.append (pad message) extension

    putStrLn $ "Legit message valid? " ++
        (show $ checkMac cipher iv message mac)
    putStrLn $ "Extended message valid? " ++
        (show $ checkMac cipher iv extMessage extMac)
