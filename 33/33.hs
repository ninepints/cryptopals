import System.Random (randomRIO)

import Util (expMod)


main :: IO ()
main = do
    testKeyExchange 37 5
    testKeyExchange bigPrime 2


bigPrime :: Integer
bigPrime = read $
    "0xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74\
      \020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f1437\
      \4fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7ed\
      \ee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf05\
      \98da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb\
      \9ed529077096966d670c354e4abc9804f1746c08ca237327ffffffffffffffff"


testKeyExchange :: Integer -> Integer -> IO ()
testKeyExchange p g = do
    putStrLn $ "p = " ++ show p
    putStrLn $ "g = " ++ show g

    a <- randomRIO (0, (p-1))
    putStrLn $ "a = " ++ show a
    b <- randomRIO (0, (p-1))
    putStrLn $ "b = " ++ show b

    let aPub = expMod g a p
        bPub = expMod g b p
        secretA = expMod bPub a p
        secretB = expMod aPub b p

    putStrLn $ "aPub = " ++ show aPub
    putStrLn $ "bPub = " ++ show bPub
    putStrLn $ "secretA = " ++ show secretA
    putStrLn $ "secretB = " ++ show secretB
