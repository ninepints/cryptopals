import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Hash.IO (hashDigestSize)
import Math.NumberTheory.Powers (integerCubeRoot)
import Text.RE.TDFA.ByteString ((?=~), compileRegex, escapeREString, matched)

import ByteFormat (integerToBytes, bytesToInteger)
import Util (hash, expMod, modInv, randomPrimeIO)


e :: Integer
e = 3


blockSize :: Int
blockSize = 192


-- Magic bytes that mean we used a SHA256 hash, according to RFC3447
asn1Stuff :: B.ByteString
asn1Stuff = B.pack $ "\x30\x31\x30\x0d\x06\x09\x60\x86\x48" ++
                     "\x01\x65\x03\x04\x02\x01\x05\x00\x04\x20"


legitMessage :: B.ByteString
legitMessage = B.pack "legit"


forgedMessage :: B.ByteString
forgedMessage = B.pack "hi mom"


main :: IO ()
main = do
    (p, q) <- (,) <$> randomPrimeIO 1024 <*> randomPrimeIO 1024

    let n = p * q
        et = (p-1) * (q-1)
        d = modInv e et
        legitSignature = signLegitimately n d
        forgedSignature = forgeSignature n
        legitValid = validate n legitMessage legitSignature
        forgedValid = validate n forgedMessage forgedSignature

    putStrLn $ "Legit signature validates? " ++ show legitValid
    putStrLn $ "Forged signature validates? " ++ show forgedValid


signLegitimately :: Integer -> Integer -> Integer
signLegitimately = flip $ expMod $ bytesToInteger $ B.concat parts
    where
        parts = [
                B.pack "\x00\x01",
                B.replicate padLength '\xff',
                B.singleton '\x00',
                asn1Stuff,
                hash SHA256 legitMessage
            ]
        padLength = blockSize - 3 - B.length asn1Stuff - hashDigestSize SHA256


-- Prepare a result bytestring, then return the largest integer f such
-- that f^3 is not greater than the integer encoding of the bytestring.
-- Since the bytestring has a bunch of 255-bytes on the end, the
-- important parts will be preserved in a slightly lesser integer.
forgeSignature :: Integer -> Integer
forgeSignature _ = integerCubeRoot $ bytesToInteger $ B.concat parts
    where
        parts = [
                B.pack "\x00\x01\xff\xff\x00",
                asn1Stuff,
                hash SHA256 forgedMessage,
                B.replicate padLength '\xff'
            ]
        padLength = blockSize - 5 - B.length asn1Stuff - hashDigestSize SHA256


validate :: Integer -> B.ByteString -> Integer -> Bool
validate n msg sig = matched $ B.append nullBytes sigCubed ?=~ fromJust regex
    where
        sigCubed = integerToBytes $ expMod sig e n
        nullBytes = B.replicate (blockSize - B.length sigCubed) '\x00'

        msgHash = hash SHA256 msg
        subRegex1 = "^\x00\x01(\xff)+\x00"
        subRegex2 = escapeREString $ B.unpack $ B.append asn1Stuff msgHash

        -- Note that we don't check for the end of the block
        regex = compileRegex $ subRegex1 ++ subRegex2
