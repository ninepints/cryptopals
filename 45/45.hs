import qualified Data.ByteString.Char8 as B
import System.Random (randomRIO)

import Crypto.Hash.Algorithms (SHA1(..))

import ByteFormat (bytesToInteger)
import Util (hash, expMod, modInv)


p :: Integer
p = read $
    "0x800000000000000089e1855218a0e7dac38136ffafa72eda7859f2171e25e65e\
      \ac698c1702578b07dc2a1076da241c76c62d374d8389ea5aeffd3226a0530cc5\
      \65f3bf6b50929139ebeac04f48c3c84afb796d61e5a4f9a8fda812ab59494232\
      \c7d2b4deb50aa18ee9e132bfa85ac4374d7f9091abc3d015efc871a584471bb1"

q :: Integer
q = 0xf4f47f05794b256174bba6e9b396a7707e563c5b


main :: IO ()
main = demo1 >> demo2


sign :: Integer -> Integer -> B.ByteString -> IO (Integer, Integer)
sign g privKey message = sign' <$> randomRIO (1, q-1)
    where sign' k = (r, s)
            where
                msgHash = bytesToInteger $ hash SHA1 message
                r = expMod g k p `mod` q
                s = ((msgHash + privKey * r) * modInv k q) `mod` q


verify :: Integer -> Integer -> Integer -> Integer -> B.ByteString -> Bool
verify g pubKey r s message = v == r
    where
        msgHash = bytesToInteger $ hash SHA1 message
        w = modInv s q
        u1 = (msgHash * w) `mod` q
        u2 = (r * w) `mod` q
        v = (expMod g u1 p * expMod pubKey u2 p) `mod` q


demo1 :: IO ()
demo1 = do
    let g = 0

    privKey <- randomRIO (1, q-1)
    _ <- sign g privKey $ B.pack "a message"

    -- The public key is always 0
    -- r = 0 will validate against any s, message, and public key
    let valid = verify g 111 0 222 $ B.pack "a different message"
    putStrLn $ "g=0: forged message validates: " ++ show valid


demo2 :: IO ()
demo2 = do
    let g = p + 1

    privKey <- randomRIO (1, q-1)
    let pubKey = expMod g privKey p
    _ <- sign g privKey $ B.pack "a message"

    -- The public key is always 1
    -- r = 1 will validate against any s and message
    let valid = verify g pubKey 1 333 $ B.pack "a different message"
    putStrLn $ "g=p+1: forged message validates: " ++ show valid
