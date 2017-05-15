import qualified Data.ByteString.Char8 as B

import Crypto.Hash.Algorithms (SHA1(..))

import ByteFormat (bytesToInteger)
import Util (getOnly, hash, expMod, modInv)


p :: Integer
p = read $
    "0x800000000000000089e1855218a0e7dac38136ffafa72eda7859f2171e25e65e\
      \ac698c1702578b07dc2a1076da241c76c62d374d8389ea5aeffd3226a0530cc5\
      \65f3bf6b50929139ebeac04f48c3c84afb796d61e5a4f9a8fda812ab59494232\
      \c7d2b4deb50aa18ee9e132bfa85ac4374d7f9091abc3d015efc871a584471bb1"

q :: Integer
q = 0xf4f47f05794b256174bba6e9b396a7707e563c5b

g :: Integer
g = read $
    "0x5958c9d3898b224b12672c0b98e06c60df923cb8bc999d119458fef538b8fa40\
      \46c8db53039db620c094c9fa077ef389b5322a559946a71903f990f1f7e0e025\
      \e2d7f7cf494aff1a0470f5b64c36b625a097f1651fe775323556fe00b3608c88\
      \7892878480e99041be601a62166ca6894bdd41a7054ec89f756ba9fc95302291"


pubKey :: Integer
pubKey = read $
    "0x84ad4719d044495496a3201c8ff484feb45b962e7302e56a392aee4abab3e4bd\
      \ebf2955b4736012f21a08084056b19bcd7fee56048e004e44984e2f411788efd\
      \c837a0d2e5abb7b555039fd243ac01f0fb2ed1dec568280ce678e931868d23eb\
      \095fde9d3779191b8c0299d6e07bbb283e6633451e535c45513b2d33c99ea17"

message :: B.ByteString
message = B.pack $
    "For those that envy a MC it can be hazardous to your health\n" ++
    "So be friendly, a matter of life and death, just like a etch-a-sketch\n"

r :: Integer
r = 548099063082341131477253921760299949438196259240

s :: Integer
s = 857042759984254168557880549501802188789837994940


main :: IO ()
main = let
    k = getOnly $ filter checkK [1..2^16-1]
    privKey = getPrivKey k
    in do
        putStrLn $ "Private key is " ++ show privKey
        putStrLn $ "Original public key is " ++ show pubKey
        putStrLn $ "New public key is .... " ++ show (expMod g privKey p)


checkK :: Integer -> Bool
checkK k = (==) r $ expMod g k p `mod` q


getPrivKey :: Integer -> Integer
getPrivKey k = ((s * k - msgHash) * modInv r q) `mod` q
    where msgHash = bytesToInteger $ hash SHA1 message
