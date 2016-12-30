import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Data.Char (chr, isSpace, ord)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import Codec.Crypto.AES (crypt, Direction(Decrypt), Mode(ECB))

import qualified ByteFormat


main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle

    let joinedContents = filter (not . isSpace) contents
        decodedContents = fromJust $ ByteFormat.b64ToBytes joinedContents
        key = SB.pack $ map (fromIntegral . ord) $ "YELLOW SUBMARINE"
        iv = SB.pack $ take 16 $ repeat 0
        plaintext = crypt ECB key iv Decrypt decodedContents

        -- There's probably a better way to do this
        paddingStatus | mod (B.length decodedContents) 16 == 0 = "Padding ok"
                      | otherwise = error "Bad padding"

    putStrLn paddingStatus
    putStrLn $ map (chr . fromIntegral) $ B.unpack plaintext
    hClose handle
