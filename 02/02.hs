import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B

import qualified ByteFormat


Just input1 = ByteFormat.hexToBytes "1c0111001f010100061a024b53535009181c"
Just input2 = ByteFormat.hexToBytes "686974207468652062756c6c277320657965"
output = B.pack $ B.zipWith xor input1 input2
main = putStrLn $ ByteFormat.bytesToHex output
