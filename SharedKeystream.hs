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
import Data.Zipper.List (next, prev, getNext, setNext, rewind, ListZipper(..))
import qualified Vigenere


type KeyTable = ListZipper (ListZipper Word8)


crackSharedKeystream :: B.ByteString a => Handle -> [a] -> IO a
crackSharedKeystream h ciphertexts = interact h ciphertexts table
    where table = newTable ciphertexts


newTable :: B.ByteString a => [a] -> KeyTable
newTable ciphertexts = ListZipper [] $ map candidatesForIndex [0..maxLen-1]
    where
        maxLen = maximum $ map B.length ciphertexts

        byteAtIndex i ct | B.length ct <= i = Nothing
                         | otherwise = Just $ B.index ct i

        bytesAtIndex i = BS.pack $ catMaybes $ map (byteAtIndex i) ciphertexts

        candidatesForIndex = ListZipper [] .
            map Vigenere.key .
            sortBy (compare `on` Vigenere.score) .
            Vigenere.guessSingleByteKey .
            bytesAtIndex


keyBytes :: KeyTable -> [Word8]
keyBytes table = map (head . getRemaining) columns
    where columns = getRemaining $ execState rewind table


nextKey :: State KeyTable (Maybe (Maybe Word8))
nextKey = moveColumn $ do
    numRemaining <- fmap (length . getRemaining) get
    if numRemaining > 1
        then next
        else return Nothing

prevKey :: State KeyTable (Maybe (Maybe Word8))
prevKey = moveColumn prev

moveColumn :: State (ListZipper Word8) a -> State KeyTable (Maybe a)
moveColumn action = do
    column <- getNext
    if isJust column
        then do
            let (result, newColumn) = runState action $ fromJust column
            setNext newColumn
            return $ Just result
        else return Nothing


interact :: B.ByteString a => Handle -> [a] -> KeyTable -> IO a
interact h ciphertexts table = do
    render ciphertexts table
    input <- hGetChar h
    if input == 'q'
        then return $ B.pack $ keyBytes table
        else let action = case input of
                    'i' -> prevKey >> return ()
                    'j' -> prev >> return ()
                    'k' -> nextKey >> return ()
                    'l' -> next >> return ()
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



