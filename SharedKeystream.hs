module SharedKeystream (crackSharedKeystream) where

import Control.Monad.State (get, execState, runState, State)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Word (Word8)
import Prelude hiding (interact)
import System.IO (hGetChar, Handle)

import qualified Data.ByteString.Common as B
import Data.List.Zipper (next, prev, getNext, setNext, rewind, ListZipper(..))
import qualified Vigenere


-- | A table used in cracking a set of ciphertexts that share a
-- keystream. A table is a 256-by-N grid of bytes, where N is the
-- length of the longest input ciphertext and each column contains the
-- values 0-255. Keystreams are cracked by focusing the correct key
-- byte in each column.
--
-- The table is implemented as a zipper of zippers, and the element
-- ahead of the cursor in each zipper is considered focused.
type KeyTable = ListZipper (ListZipper Word8)


-- | Displays a text-based interactive interface for cracking a set of
-- ciphertexts that share a keystream. Input is read from the provided
-- handle, while output is printed to stdout.
crackSharedKeystream :: B.ByteString a => Bool -> Handle -> [a] -> IO a
crackSharedKeystream easyMode h ciphertexts = interact h ciphertexts table
    where table = newTable easyMode ciphertexts


-- | Initializes a new key table. The boolean @easyMode@ parameter
-- dictates whether the bytes in each column will be initialized in
-- ascending order (@easyMode == False@) or with probable key bytes
-- first, based on the distance of the resulting plaintext from English
-- (@easyMode == True@).
newTable :: B.ByteString a => Bool -> [a] -> KeyTable
newTable easyMode ciphertexts = zipper $ map candidatesForIndex [0..maxLen-1]
    where
        zipper = ListZipper []
        maxLen = maximum $ map B.length ciphertexts

        byteAtIndex i ct | B.length ct <= i = Nothing
                         | otherwise = Just $ B.index ct i
        bytesAtIndex i = BS.pack $ catMaybes $ map (byteAtIndex i) ciphertexts

        candidatesForIndex = zipper . if easyMode
            then map Vigenere.key .
                 sortBy (compare `on` Vigenere.score) .
                 Vigenere.guessSingleByteKey .
                 bytesAtIndex
            else const [0..255]


-- | Extracts the focused key bytes from a key table.
keyBytes :: KeyTable -> [Word8]
keyBytes table = map (head . getRemaining) columns
    where columns = getRemaining $ execState rewind table


-- | Focuses the next key byte in the focused column of a key table.
nextKey :: State KeyTable (Maybe Word8)
nextKey = moveColumn $ do
    numRemaining <- fmap (length . getRemaining) get
    if numRemaining > 1
        then next
        else return Nothing  -- Don't advance past last byte

-- | Focuses the previous key byte in the focused column of a key table.
prevKey :: State KeyTable (Maybe Word8)
prevKey = moveColumn prev

-- | Applies a column action to the focused column in a key table.
moveColumn :: State (ListZipper Word8) (Maybe a) -> State KeyTable (Maybe a)
moveColumn action = do
    column <- getNext
    if isJust column
        then do
            let (result, newColumn) = runState action $ fromJust column
            setNext newColumn
            return result
        else return Nothing


interact :: B.ByteString a => Handle -> [a] -> KeyTable -> IO a
interact h ciphertexts table = do
    render ciphertexts table
    input <- hGetChar h
    if input == 'q'
        then return $ B.pack $ keyBytes table
        else let action = case input of
                    'w' -> prevKey >> return ()
                    'a' -> prev >> return ()
                    's' -> nextKey >> return ()
                    'd' -> next >> return ()
                    _ -> return ()
            in interact h ciphertexts $ execState action table


-- I was gonna use ncurses for this but I can't get it to compile
render :: B.ByteString a => [a] -> KeyTable -> IO ()
render ciphertexts table = do
    let kb = keyBytes table
    putStrLn "\ESC[2J"  -- Escape sequence to clear the terminal
    putStrLn "Key:"
    printBytes $ localElements 6 $ fmap (head . getRemaining) table
    putStrLn "Plaintexts:"
    let i = length $ getConsumed table
        plaintexts = map (zipWith xor kb . B.unpack) ciphertexts
        localPlaintexts = map (drop (i - 6) . take (i + 6)) plaintexts
    sequence_ $ map printBytes localPlaintexts


-- | Returns a list containing N elements to either side of the cursor
-- in a zipper (up to 2*N total elements).
localElements :: Integer -> ListZipper a -> [a]
localElements n (ListZipper ys xs) = getRemaining $ execState rewind localZipper
    where
        n' = fromIntegral n
        localZipper = ListZipper (take n' ys) (take n' xs)


printBytes :: [Word8] -> IO ()
printBytes bytes = putStrLn $ intercalate " " $ map showPadded bytes
    where
        pad byte = (chr $ fromIntegral byte) : repeat ' '
        showPadded byte = take 4 $ drop 1 $ show $ pad byte



