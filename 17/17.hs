import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (isJust)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (blockSize, BlockCipher)

import BlockCipher (cbcEncrypt, cbcDecrypt)
import ByteFormat (base64ToBytes)
import Data.Chunkable (chunksOf)
import Data.List.Window (window)
import Padding (pkcs7pad, pkcs7unpad)
import Util (getOnly, randomBytesIO, randomlyKeyedCipherIO, xorBytes)


secrets :: [BS.ByteString]
Just secrets = sequence $ map (base64ToBytes . BC.pack) [
        "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
        "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW" ++
            "4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
        "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
        "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
        "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
        "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
        "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
        "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
        "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
        "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
    ]


encrypt :: BlockCipher a =>
    a -> BS.ByteString -> BS.ByteString -> BS.ByteString
encrypt cipher iv input = cbcEncrypt cipher iv $ pad input
    where pad = pkcs7pad $ fromIntegral $ blockSize cipher


decrypt :: BlockCipher a => a -> BS.ByteString -> BS.ByteString -> Bool
decrypt cipher iv = isJust . pkcs7unpad . cbcDecrypt cipher iv


crackBlock :: (BS.ByteString -> Bool) ->
    BS.ByteString -> BS.ByteString -> BS.ByteString
crackBlock oracle prevBlock block = crackNextByte [BS.empty]
    where
        nTotal = BS.length block

        crackNextByte solutions | done = getOnly solutions
                                | otherwise = crackNextByte solutions'
            where
                done = BS.length (head solutions) == nTotal
                solutions' = solutions >>= crackNextByte'

        crackNextByte' solution = map (flip BS.cons $ solution) nextBytes
            where
                nSolved = BS.length solution
                nPadding = nTotal - nSolved - 1
                paddingByte = fromIntegral $ nSolved + 1
                ctextByte = BS.index prevBlock nPadding

                makeMask byte = (byte, BS.concat [
                        BS.replicate nPadding 0,
                        BS.singleton byte,
                        xorBytes (BS.drop (nPadding + 1) prevBlock) $
                            xorBytes solution $
                            BS.replicate nSolved paddingByte
                    ])
                masks = map makeMask [0..255]

                isSuccess mask = oracle $ BS.append (snd mask) block
                getPlaintext mask = xor paddingByte $ xor (fst mask) ctextByte
                nextBytes = map getPlaintext $ filter isSuccess masks


crackSecret :: (BS.ByteString -> Bool) ->
    BS.ByteString -> BS.ByteString -> BS.ByteString
crackSecret oracle iv secret = BS.concat $ map crackPair pairs
    where
        pairs = window 2 $ (:) iv $ chunksOf 16 secret
        crackPair [x,y] = crackBlock oracle x y


main :: IO ()
main = do
    cipher <- randomlyKeyedCipherIO :: IO AES128
    iv <- randomBytesIO $ fromIntegral $ blockSize cipher

    let encrypt' = encrypt cipher iv
        decrypt' = decrypt cipher iv
        encryptAndCrack = pkcs7unpad . crackSecret decrypt' iv . encrypt'

    sequence_ $ map (print . encryptAndCrack) secrets
