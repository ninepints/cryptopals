import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)

import qualified ByteFormat


input = "Burning 'em, if you ain't quick and nimble\n" ++
    "I go crazy when I hear a cymbal"
plaintext = map (fromIntegral . ord) input
key = map (fromIntegral . ord) $ cycle "ICE"
result = zipWith xor plaintext key
main = putStrLn $ ByteFormat.bytesToHex $ B.pack result
