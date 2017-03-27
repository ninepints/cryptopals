import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Tuple (uncurry)
import System.Random (randomRIO)

import qualified SpecImplementations.SHA1 as SHA1
import Util (randomBytesIO)


origMsg :: B.ByteString
origMsg = B.pack $
    "comment1=cooking MCs;userdata=foo;comment2= like a pound of bacon"


validate :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
validate secret = (==) . SHA1.hash . B.append secret


tamper :: B.ByteString -> Int -> (B.ByteString, B.ByteString)
tamper origMac secretLength = (newMsg, newMac)
    where
        prefixedMsg = B.append (B.replicate secretLength 'a') origMsg
        prefixedMsgLength = fromIntegral $ B.length prefixedMsg
        paddedPrefixedMsg = SHA1.pad (8 * prefixedMsgLength) prefixedMsg
        paddedPrefixedMsgLength = fromIntegral $ B.length paddedPrefixedMsg

        addition = B.pack ";admin=true"
        newMsg = B.drop secretLength $ B.append paddedPrefixedMsg addition
        context = fromJust $ SHA1.buildContext origMac paddedPrefixedMsgLength
        newMac = SHA1.finalize $ SHA1.update context addition


main :: IO ()
main = do
    secretLength <- randomRIO (10, 20)
    secret <- randomBytesIO secretLength

    let origMac = SHA1.hash $ B.append secret origMsg
        origValid = validate secret origMsg origMac

    putStrLn $ "Original message: " ++ show origMsg
    putStrLn $ "Original MAC: " ++ show origMac
    putStrLn $ "Original MAC is actually valid: " ++ show origValid

    let attempts = map (tamper origMac) [10..20]
        [(newMsg, newMac)] = filter (uncurry $ validate secret) attempts

    putStrLn $ "Altered message: " ++ show newMsg
    putStrLn $ "Altered MAC: " ++ show newMac
