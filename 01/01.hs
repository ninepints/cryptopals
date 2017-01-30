import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)

import qualified ByteFormat


input :: ByteString
input = fromString $
    "49276d206b696c6c696e6720796f757220627261696e206c" ++
    "696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: ByteString -> Maybe ByteString
hexToBase64 = fmap ByteFormat.bytesToBase64 . ByteFormat.hexToBytes

main :: IO ()
main = putStrLn $ show $ hexToBase64 input
