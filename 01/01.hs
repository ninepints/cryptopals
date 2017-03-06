import Data.ByteString.Char8 (pack, ByteString)

import ByteFormat (bytesToBase64, hexToBytes)


input :: ByteString
input = pack $
    "49276d206b696c6c696e6720796f757220627261696e206c" ++
    "696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: ByteString -> Maybe ByteString
hexToBase64 = fmap ByteFormat.bytesToBase64 . ByteFormat.hexToBytes

main :: IO ()
main = print $ hexToBase64 input
