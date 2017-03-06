import Data.ByteString.Char8 (pack)

import Padding (pkcs7pad)


main :: IO ()
main = print $ pkcs7pad 20 $ pack "YELLOW SUBMARINE"
