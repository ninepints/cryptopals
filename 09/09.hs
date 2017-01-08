import qualified Data.ByteString.Lazy as B
import Data.Char (ord)

import qualified Padding


input = B.pack $ map (fromIntegral . ord) "YELLOW SUBMARINE"
main = putStrLn $ show $ Padding.pkcs7pad 20 input
