import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import Text.Printf (printf)

import ByteFormat (bytesToHex)
import qualified SpecImplementations.SHA1 as SHA1


messages :: [B.ByteString]
messages = map B.pack [
        "message",
        "message longer than 64 characters to make sure that works properly"
    ]


main :: IO ()
main = sequence_ $ map f messages
    where f message = putStrLn $ printf "%s SHA1(%s)" hash (show message)
            where hash = B.unpack $ bytesToHex $ SHA1.hash message
