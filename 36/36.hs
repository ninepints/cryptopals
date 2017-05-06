import qualified Data.ByteString.Char8 as B
import System.Random (randomRIO)

import Crypto.Hash.Algorithms (SHA256(..))

import ByteFormat (bytesToInteger, integerToBytes)
import HMAC (hmac)
import Padding (constantPad)
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

k :: Integer
k = 3


main :: IO ()
main = do
    password <- randomBytesIO 16
    aPriv <- randomRIO (0, (p-1))
    bPriv <- randomRIO (0, (p-1))

    -- On the client, beforehand - salt, v, and username sent to server
    (salt, v) <- genSaltVerifier password

    -- On the client - aPub and username sent to server
    let aPub = expMod g aPriv p

    -- On the server - bPub, salt returned to client
    -- v, salt retrieved based on username
    let bPub = (expMod g bPriv p + k * v) `mod` p

    let u = fromBytes $ hash SHA256 $ B.concat $ map toBytes [aPub, bPub]
        aKey = getClientKey aPriv bPub u salt password
        bKey = getServerKey bPriv aPub u v
        aHmac = hmac SHA256 aKey salt
        bHmac = hmac SHA256 bKey salt

    putStrLn $ "HMACs match? " ++ show (aHmac == bHmac)


genSaltVerifier :: B.ByteString -> IO (B.ByteString, Integer)
genSaltVerifier password = do
    salt <- randomBytesIO 8
    let x = saltPasswordToInt salt password
    return (salt, expMod g x p)


getClientKey :: Integer -> Integer -> Integer ->
    B.ByteString -> B.ByteString -> B.ByteString
getClientKey aPriv bPub u salt password = toBytes $ expMod base e p
    where
        x = saltPasswordToInt salt password
        base = bPub - k * expMod g x p
        e = aPriv + u * x


getServerKey :: Integer -> Integer -> Integer -> Integer -> B.ByteString
getServerKey bPriv aPub u v = toBytes $ expMod base bPriv p
    where base = aPub * expMod v u p


saltPasswordToInt :: B.ByteString -> B.ByteString -> Integer
saltPasswordToInt = curry $ fromBytes . hash SHA256 . uncurry B.append


fromBytes :: B.ByteString -> Integer
fromBytes = bytesToInteger

toBytes :: Integer -> B.ByteString
toBytes = hash SHA256 . padToLengthOfP . integerToBytes
    where padToLengthOfP = constantPad 192 0

