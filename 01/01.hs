import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.String (fromString)

import qualified ByteFormat


input :: ByteString
input = fromString $
    "49276d206b696c6c696e6720796f757220627261696e206c" ++
    "696b65206120706f69736f6e6f7573206d757368726f6f6d"

main :: IO ()
main = putStrLn $ show $ fromJust $ ByteFormat.hexToBase64 input
