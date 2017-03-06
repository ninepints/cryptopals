import Data.ByteString.Char8 (pack)

import Padding (pkcs7unpad)


main :: IO ()
main = do
    print $ pkcs7unpad $ pack "ICE ICE BABY\x04\x04\x04\x04"
    print $ pkcs7unpad $ pack "ICE ICE BABY\x05\x05\x05\x05"
    print $ pkcs7unpad $ pack "ICE ICE BABY\x01\x02\x03\x04"
