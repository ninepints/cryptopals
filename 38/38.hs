import qualified Data.ByteString.Char8 as B
import System.Random (randomRIO)

import Crypto.Hash.Algorithms (SHA256(..))

import ByteFormat (bytesToInteger, integerToBytes)
import HMAC (hmac)
import Padding (constantPad)
import Util (expMod, hash, randomChoiceIO)


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


-- I think the point of this exercise is that in proper SRP, the
-- server's ephemeral public key depends on the stored verifier,
-- which in turn depends on the password. Anyone impersonating the
-- server doesn't have a correctly-derived public key, so the HMAC
-- sent by the client can't be used to check passwords.
--
-- A third party listening in on proper SRP won't have any private
-- keys, so they won't be able to check passwords either.
--
-- In this "simplified" version of SRP, we can impersonate the server
-- and use whatever private key we want and the client will still send
-- us useful information. To speed things up, we're pretending that
-- the client's password options are limited to the list below.
passwords :: [B.ByteString]
passwords = map B.pack [
        "password",
        "qwerty",
        "hunter2"
    ]


main :: IO ()
main = do
    password <- randomChoiceIO passwords
    putStrLn $ "Actual password is " ++ show password

    aPriv <- randomRIO (0, (p-1))
    bPriv <- randomRIO (0, (p-1))

    -- On the client - aPub and username sent to server
    let aPub = expMod g aPriv p

    -- On a different, malicious server - bPub, salt, u returned to client
    -- We can't supply a degenerate public key or we wouldn't get a useful
    -- response from the client, but the salt and u can be chosen to make
    -- password checking as fast as possible
    let bPub = expMod g bPriv p
        salt = B.empty
        u = 1

    let aKey = getClientKey aPriv bPub u salt password
        aHmac = hmac SHA256 aKey salt
        password' = guessPassword aPub bPriv u salt aHmac

    putStrLn $ "Recovered password " ++ show password'


getClientKey :: Integer -> Integer -> Integer ->
    B.ByteString -> B.ByteString -> B.ByteString
getClientKey aPriv bPub u salt password = toBytes $ expMod bPub e p
    where
        x = saltPasswordToInt salt password
        e = aPriv + u * x


guessPassword :: Integer -> Integer -> Integer ->
    B.ByteString -> B.ByteString -> B.ByteString
guessPassword aPub bPriv u salt aHmac = validPassword
    where
        [validPassword] = filter isValid passwords
        isValid password = bHmac == aHmac
            where
                x = saltPasswordToInt salt password
                v = expMod g x p
                bKey = toBytes $ expMod (aPub * expMod v u p) bPriv p
                bHmac = hmac SHA256 bKey salt


saltPasswordToInt :: B.ByteString -> B.ByteString -> Integer
saltPasswordToInt = curry $ fromBytes . hash SHA256 . uncurry B.append


fromBytes :: B.ByteString -> Integer
fromBytes = bytesToInteger

toBytes :: Integer -> B.ByteString
toBytes = hash SHA256 . padToLengthOfP . integerToBytes
    where padToLengthOfP = constantPad 192 0

