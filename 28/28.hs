import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import Text.Printf (printf)

import ByteFormat (bytesToHex)
import qualified SpecImplementations.HashCommon as HC
import SpecImplementations.MD4 (MD4(..))
import SpecImplementations.SHA1 (SHA1(..))


messages :: [B.ByteString]
messages = map B.pack [
        "message",
        "message longer than 64 characters to make sure that works properly"
    ]


putMessage :: HC.HashImpl h a => h -> B.ByteString -> IO ()
putMessage hashImpl message = putStrLn output
    where
        output = printf "%s %s(%s)" hash (show hashImpl) (show message)
        hash = B.unpack $ bytesToHex $ HC.hash hashImpl message


main :: IO ()
main = do
    [hashName] <- getArgs
    case hashName of
        "MD4" -> sequence_ $ map (putMessage MD4) messages
        "SHA1" -> sequence_ $ map (putMessage SHA1) messages
