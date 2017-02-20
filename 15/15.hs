import Data.ByteString (ByteString)
import Data.String (fromString)

import Padding (pkcs7unpad)


main :: IO ()
main = do
    let input1 = fromString "ICE ICE BABY\x04\x04\x04\x04" :: ByteString
        input2 = fromString "ICE ICE BABY\x05\x05\x05\x05" :: ByteString
        input3 = fromString "ICE ICE BABY\x01\x02\x03\x04" :: ByteString

    print $ pkcs7unpad input1
    print $ pkcs7unpad input2
    print $ pkcs7unpad input3
