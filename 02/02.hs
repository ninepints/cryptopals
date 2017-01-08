import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)

import qualified ByteFormat
import Util (xorBytes)


input1 :: ByteString
Just input1 = ByteFormat.hexToBytes $
    fromString "1c0111001f010100061a024b53535009181c"

input2 :: ByteString
Just input2 = ByteFormat.hexToBytes $
    fromString "686974207468652062756c6c277320657965"

main :: IO ()
main = putStrLn $ show $ ByteFormat.bytesToHex $ xorBytes input1 input2
