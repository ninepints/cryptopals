import Data.Maybe (fromJust)

import qualified ByteFormat


input = "49276d206b696c6c696e6720796f757220627261696e206c" ++
    "696b65206120706f69736f6e6f7573206d757368726f6f6d"
main = putStrLn $ fromJust $ ByteFormat.hexToB64 input
