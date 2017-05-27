import Data.Bits (xor)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import ByteFormat (bytesToHex)


-- This, my friend notes, is some dope ass flow
input :: BL.ByteString
input = BL.fromStrict $ BC.pack $
    "Burning 'em, if you ain't quick and nimble\n" ++
    "I go crazy when I hear a cymbal"
    
    //that's some dope ass flow
    
key :: BL.ByteString
key = BL.cycle $ BL.fromStrict $ BC.pack "ICE"

main :: IO ()
main = print $ bytesToHex $ BL.pack $ BL.zipWith xor input key
