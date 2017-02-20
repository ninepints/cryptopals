import Data.Bits (xor)
import qualified Data.ByteString.Lazy as B
import Data.String (fromString)

import qualified ByteFormat


input :: B.ByteString
input = fromString $
    "Burning 'em, if you ain't quick and nimble\n" ++
    "I go crazy when I hear a cymbal"

key :: B.ByteString
key = B.cycle $ fromString "ICE"

main :: IO ()
main = print $ ByteFormat.bytesToHex $ B.pack $ B.zipWith xor input key
