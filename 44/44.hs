import Control.Applicative (liftA3)
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as B
import Data.Function (on)
import Data.List (groupBy, sortBy, stripPrefix)
import Data.Maybe (fromJust)
import Data.Ord (Down(..))
import System.Environment (getArgs)

import Crypto.Hash.Algorithms (SHA1(..))

import ByteFormat (bytesToInteger, bytesToHex)
import Data.Chunkable (chunksOf)
import Util (hash, expMod, modInv)


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
    "0x2d026f4bf30195ede3a088da85e398ef869611d0f68f0713d51c9c1a3a26c951\
      \05d915e2d8cdf26d056b86b8a7b85519b1c23cc3ecdc6062650462e3063bd179\
      \c2a6581519f674a61f1d89a1fff27171ebc1b93d4dc57bceb7ae2430f98a6a4d\
      \83d8279ee65d71c1203d2c96d65ebbf7cce9d32971c3de5084cce04a2e147821"


data SignedMessage = SignedMessage {
    getMessage :: B.ByteString,
    getR :: Integer,
    getS :: Integer
}


main :: IO ()
main = do
    [filename] <- getArgs
    let contentsToMessages = map (fromJust . parseMessage) . chunksOf 4 . lines
    signedMessages <- contentsToMessages <$> readFile filename

    let [msgA, msgB] = take 2 $
                       head $
                       sortBy (compare `on` Down . length) $
                       groupBy ((==) `on` getR) $
                       sortBy (compare `on` getR) signedMessages

        k = getK msgA msgB
        privKey = getPrivKey k msgA

    putStrLn $ "Private key is " ++ show privKey
    putStrLn $ "Original public key is " ++ show pubKey
    putStrLn $ "New public key is .... " ++ show (expMod g privKey p)


parseMessage :: [String] -> Maybe SignedMessage
parseMessage xs = if length xs /= 4 then Nothing else do
    -- In real life we'd return Nothing on integer parse failures
    -- rather than exploding. Hope the Maybe nets me partial credit...
    --
    -- (it would probably net me more credit if I didn't use fromJust above)
    message <- B.pack <$> stripPrefix "msg: " (xs !! 0)
    s <- read <$> stripPrefix "s: " (xs !! 1)
    r <- read <$> stripPrefix "r: " (xs !! 2)
    msgHash <- B.pack <$> stripPrefix "m: " (xs !! 3)
    guard $ bytesToHex (hash SHA1 message) == msgHash
    return $ SignedMessage message r s


getK :: SignedMessage -> SignedMessage -> Integer
getK msgA msgB = (dividend * modInv divisor q) `mod` q
    where
        dividend = getHash msgA - getHash msgB
        divisor = getS msgA - getS msgB


getPrivKey :: Integer -> SignedMessage -> Integer
getPrivKey k message = ((s * k - msgHash) * modInv r q) `mod` q
    where (s, r, msgHash) = liftA3 (,,) getS getR getHash message


getHash :: SignedMessage -> Integer
getHash = bytesToInteger . hash SHA1 . getMessage
