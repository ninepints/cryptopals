import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)

import Padding (pkcs7pad)


main :: IO ()
main = putStrLn $ show $ pkcs7pad 20 input
    where input = fromString "YELLOW SUBMARINE" :: ByteString
