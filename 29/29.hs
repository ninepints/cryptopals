import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Tuple (uncurry)
import System.Environment (getArgs)
import System.Random (randomRIO)

import Util (randomBytesIO)
import qualified SpecImplementations.HashCommon as HC
import SpecImplementations.MD4 (MD4(..))
import SpecImplementations.SHA1 (SHA1(..))


origMsg :: B.ByteString
origMsg = B.pack $
    "comment1=cooking MCs;userdata=foo;comment2= like a pound of bacon"


validate :: HC.HashImpl h a =>
    h -> B.ByteString -> B.ByteString -> B.ByteString -> Bool
validate hashImpl secret = (==) . HC.hash hashImpl . B.append secret


tamper :: HC.HashImpl h a =>
    h -> B.ByteString -> Int -> (B.ByteString, B.ByteString)
tamper hashImpl origMac secretLength = (newMsg, newMac)
    where
        pad = HC.pad hashImpl
        buildContext = HC.buildContext hashImpl
        update = HC.update hashImpl
        finalize = HC.finalize hashImpl

        prefixedMsg = B.append (B.replicate secretLength 'a') origMsg
        prefixedMsgLength = fromIntegral $ B.length prefixedMsg
        paddedPrefixedMsg = pad (8 * prefixedMsgLength) prefixedMsg
        paddedPrefixedMsgLength = fromIntegral $ B.length paddedPrefixedMsg

        addition = B.pack ";admin=true"
        newMsg = B.drop secretLength $ B.append paddedPrefixedMsg addition
        context = fromJust $ buildContext origMac paddedPrefixedMsgLength
        newMac = finalize $ update context addition


main' :: HC.HashImpl h a => h -> IO ()
main' hashImpl = do
    secretLength <- randomRIO (10, 20)
    secret <- randomBytesIO secretLength

    let validate' = validate hashImpl
        tamper' = tamper hashImpl
        origMac = HC.hash hashImpl $ B.append secret origMsg
        origValid = validate' secret origMsg origMac

    putStrLn $ "Original message: " ++ show origMsg
    putStrLn $ "Original MAC: " ++ show origMac
    putStrLn $ "Original message/MAC valid: " ++ show origValid

    let attempts = map (tamper' origMac) [10..20]
        [(newMsg, newMac)] = filter (uncurry $ validate' secret) attempts

    putStrLn "Successfully altered message/MAC maintaining validity!"
    putStrLn $ "Altered message: " ++ show newMsg
    putStrLn $ "Altered MAC: " ++ show newMac


main :: IO ()
main = do
    [hashName] <- getArgs
    case hashName of
        "MD4" -> main' MD4
        "SHA1" -> main' SHA1
