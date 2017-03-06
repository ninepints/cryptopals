import Data.ByteString.Char8 (pack, ByteString)

import ByteFormat (bytesToHex, hexToBytes)
import Util (xorBytes)


input1 :: ByteString
Just input1 = hexToBytes $ pack "1c0111001f010100061a024b53535009181c"

input2 :: ByteString
Just input2 = hexToBytes $ pack "686974207468652062756c6c277320657965"

main :: IO ()
main = print $ bytesToHex $ xorBytes input1 input2
