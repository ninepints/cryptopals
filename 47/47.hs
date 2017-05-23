import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Function (on)
import Data.List (sortBy)
import Data.Tuple (swap)
import Text.Printf (printf)

import ByteFormat (integerToBytes, bytesToInteger)
import Padding (constantPadLeft, pkcs1_15padIO, pkcs1_15unpad)
import Util (expMod, modInv, randomPrimeIO)


e :: Integer
e = 3


paddingValid :: Integer -> Integer -> Integer -> Integer -> Bool
paddingValid n k d ciphertext = B.isPrefixOf (B.pack [0, 2]) $
    constantPadLeft k 0 $ integerToBytes $ expMod ciphertext d n


main :: IO ()
main = do
    (p, q) <- (,) <$> randomPrimeIO 384 <*> randomPrimeIO 384

    let n = p * q
        et = (p-1) * (q-1)
        d = modInv e et
        k = let f ck = 2 ^ (8 * (ck - 1)) < n in last $ takeWhile f [1..]

    plaintext <- bytesToInteger <$> (pkcs1_15padIO k $ BC.pack "kick it, CC")

    let ciphertext = expMod plaintext e n
        plaintext' = findPlaintext n k (paddingValid n k d) ciphertext
        integerToMessage :: Integer -> Maybe B.ByteString
        integerToMessage = pkcs1_15unpad . constantPadLeft k 0 . integerToBytes

    Just message <- integerToMessage <$> plaintext'
    putStrLn $ "Recovered message " ++ show message


findPlaintext :: Integer -> Integer ->
    (Integer -> Bool) -> Integer -> IO Integer
findPlaintext n k oracle ciphertext = find s0 undefined [(2*b, 3*b - 1)]
    where
        s0 = 1 :: Integer
        b = 2 ^ (8 * (k - 2))

        combineIntervals = doCombine . doSort . filterEmpty
            where
                doCombine ((l1,u1) : (l2,u2) : xs) = if l2 <= u1 + 1
                    then doCombine ((l1,u2) : xs)
                    else (l1,u1) : doCombine ((l2,u2) : xs)
                doCombine xs = xs
                doSort = sortBy (compare `on` fst)
                filterEmpty = filter $ uncurry (<=)

        getNewIntervals s intervals = combineIntervals $ do
            (l,u) <- intervals
            r <- [(l*s - 3*b + 1) `div` n .. (u*s - 2*b) `div` n]
            let (l', lmod) = (2*b + r*n) `divMod` s
                u' = (3*b - 1 + r*n) `div` s
            return (max l $ l' + if lmod == 0 then 0 else 1, min u u')

        getFirstS = head . filter (oracle . multByCiphertext)
            where multByCiphertext s = (ciphertext * expMod s e n) `mod` n

        recurseOnCandidates i intervals cs = let
            fmt = "Iter %u: %u intervals with total range %u"
            totalRange = sum $ map ((+1) . uncurry (-) . swap) intervals
            s' = getFirstS cs
            intervals' = getNewIntervals s' intervals
            in do
                putStrLn $ printf fmt i (length intervals) totalRange
                putStrLn $ "Found s " ++ show s'
                find (i+1) s' intervals'

        find _ _ [(l,u)] | u == l = return l
        find i@1 _ intervals = recurseOnCandidates i intervals cs
            where cs = [n `div` (3 * b) ..]
        find i s intervals@[(l,u)] = recurseOnCandidates i intervals cs
            where cs = do
                    r <- [2 * (u*s - 2*b) `div` n ..]
                    [(2*b + r*n) `div` u .. (3*b + r*n) `div` l]
        find i s intervals = recurseOnCandidates i intervals [s+1..]
