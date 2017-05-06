import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import System.Random (randomRIO)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash.Algorithms (SHA1(..))

import BlockCipher (cbcEncrypt, cbcDecrypt)
import ByteFormat (integerToBytes)
import Padding (constantPad, pkcs7pad)
import Util (expMod, hash, randomBytesIO)


p :: Integer
p = read $
    "0xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74\
      \020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f1437\
      \4fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7ed\
      \ee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf05\
      \98da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb\
      \9ed529077096966d670c354e4abc9804f1746c08ca237327ffffffffffffffff"


main :: IO ()
main = for_ [1, p, p - 1] doKeyExchange


doKeyExchange :: Integer -> IO ()
doKeyExchange g = do
    putStrLn $ "p = " ++ show p
    putStrLn $ "g = " ++ show g

    aPriv <- randomRIO (0, (p-1))
    bPriv <- randomRIO (0, (p-1))

    let aPub = expMod g aPriv p
        bPub = expMod g bPriv p
        sA = expMod bPub aPriv p
        sB = expMod aPub bPriv p

        -- When g == p - 1, the shared secret is 1 75% of the time and
        -- p - 1 the remaining 25% of the time. We'll try both.
        (sMA, sMB) = case g of
            1 -> ([bPub], [aPub])
            x | x == p -> ([0], [0])
            x | x == p - 1 -> ([1, p - 1], [1, p - 1])

        padToLengthOfP = constantPad 192 0 :: B.ByteString -> B.ByteString
        getKey = B.take 16 . hash SHA1 . padToLengthOfP . integerToBytes
        unwrap (CryptoPassed x) = x :: AES128

        cipherA = unwrap $ cipherInit $ getKey sA
        cipherB = unwrap $ cipherInit $ getKey sB
        cipherMA = map (unwrap . cipherInit . getKey) sMA
        cipherMB = map (unwrap . cipherInit . getKey) sMB

        message = pkcs7pad 16 $ B.pack "message!"

    ivA <- randomBytesIO 16
    ivB <- randomBytesIO 16

    let decryptA cipher = cbcDecrypt cipher ivA encryptedMessageA
        decryptB cipher = cbcDecrypt cipher ivB encryptedMessageB

        encryptedMessageA = cbcEncrypt cipherA ivA message
        decryptedMessageAB = decryptA cipherB
        decryptedMessageAM = map decryptA cipherMB

        encryptedMessageB = cbcEncrypt cipherB ivB message
        decryptedMessageBA = decryptB cipherA
        decryptedMessageBM = map decryptB cipherMA

    putStrLn $ "A's message to B: " ++ show encryptedMessageA
    putStrLn $ "...decrypted by B: " ++ show decryptedMessageAB
    for_ decryptedMessageAM $ (\x ->
        putStrLn $ "...decrypted by M: " ++ show x)

    putStrLn $ "B's response to A: " ++ show encryptedMessageB
    putStrLn $ "...decrypted by A: " ++ show decryptedMessageBA
    for_ decryptedMessageBM $ (\x ->
        putStrLn $ "...decrypted by M: " ++ show x)
