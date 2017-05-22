import qualified Data.ByteString.Char8 as B
import System.Random (randomRIO)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash.Algorithms (SHA1(..))

import BlockCipher (cbcEncrypt, cbcDecrypt)
import ByteFormat (integerToBytes)
import Padding (pkcs7pad)
import Util (expMod, hash, randomBytesIO)


p :: Integer
p = read $
    "0xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74\
      \020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f1437\
      \4fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7ed\
      \ee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf05\
      \98da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb\
      \9ed529077096966d670c354e4abc9804f1746c08ca237327ffffffffffffffff"

g :: Integer
g = 2


main :: IO ()
main = do
    aPriv <- randomRIO (0, (p-1))
    bPriv <- randomRIO (0, (p-1))

    -- All shared secrets are the same, but separated for bookkeeping
    let sA = expMod p aPriv p
        sB = expMod p bPriv p
        sM = 0

        getKey = B.take 16 . hash SHA1 . integerToBytes

        CryptoPassed cipherA = cipherInit $ getKey sA :: CryptoFailable AES128
        CryptoPassed cipherB = cipherInit $ getKey sB :: CryptoFailable AES128
        CryptoPassed cipherM = cipherInit $ getKey sM :: CryptoFailable AES128

        message = pkcs7pad 16 $ B.pack "message!"

    ivA <- randomBytesIO 16
    ivB <- randomBytesIO 16

    let encryptedMessageA = cbcEncrypt cipherA ivA message
        decryptedMessageAB = cbcDecrypt cipherB ivA encryptedMessageA
        decryptedMessageAM = cbcDecrypt cipherM ivA encryptedMessageA

        encryptedMessageB = cbcEncrypt cipherB ivB message
        decryptedMessageBA = cbcDecrypt cipherA ivB encryptedMessageB
        decryptedMessageBM = cbcDecrypt cipherM ivB encryptedMessageB

    putStrLn $ "A's message to B: " ++ show encryptedMessageA
    putStrLn $ "...decrypted by B: " ++ show decryptedMessageAB
    putStrLn $ "...decrypted by M: " ++ show decryptedMessageAM

    putStrLn $ "B's response to A: " ++ show encryptedMessageB
    putStrLn $ "...decrypted by A: " ++ show decryptedMessageBA
    putStrLn $ "...decrypted by M: " ++ show decryptedMessageBM
