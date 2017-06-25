module Data.List.Zipper (
    next,
    prev,
    getNext,
    getPrev,
    setNext,
    setPrev,
    hasNext,
    hasPrev,
    rewind,
    ListZipper(..)
) where

import Control.Monad.State (gets, modify, state, State)
import Data.Maybe (listToMaybe)


-- | A zipper for lists, akin to Java's ListIterator. A zipper can be
-- thought of as a cursor within a list, positioned between elements or
-- before the first element or after the last.
data ListZipper a = ListZipper {
    getConsumed :: [a],
    getRemaining :: [a]
} deriving (Show)

instance Functor ListZipper where
    fmap f (ListZipper ys xs) = ListZipper (map f ys) (map f xs)


-- | Returns the element ahead of the cursor and moves the cursor
-- forwards. If the cursor is already at the end of the list,
-- 'Nothing' is returned and the position of the cursor is unchanged.
next :: State (ListZipper a) (Maybe a)
next = state $ \zipper -> case zipper of
    ListZipper ys (x:xs) -> (Just x, ListZipper (x:ys) xs)
    _ -> (Nothing, zipper)

-- | Returns the element behind the cursor and moves the cursor
-- backwards. If the cursor is already at the beginning of the list,
-- 'Nothing' is returned and the position of the cursor is unchanged.
prev :: State (ListZipper a) (Maybe a)
prev = state $ \zipper -> case zipper of
    ListZipper (y:ys) xs -> (Just y, ListZipper ys (y:xs))
    _ -> (Nothing, zipper)


-- | Indicates whether the cursor can be moved forwards.
hasNext :: State (ListZipper a) Bool
hasNext = gets $ not . null . getRemaining

-- | Indicates whether the cursor can be moved backwards.
hasPrev :: State (ListZipper a) Bool
hasPrev = gets $ not . null . getConsumed

-- | Returns the element ahead of the cursor, if any.
getNext :: State (ListZipper a) (Maybe a)
getNext = gets $ listToMaybe . getRemaining

-- | Returns the element behind the cursor, if any.
getPrev :: State (ListZipper a) (Maybe a)
getPrev = gets $ listToMaybe . getConsumed

-- | Sets the element ahead of the cursor.
-- This requires that the cursor not be at the end of the list.
setNext :: a -> State (ListZipper a) ()
setNext x = modify $ \(ListZipper ys (_:xs)) -> ListZipper ys (x:xs)

-- | Sets the element behind the cursor.
-- This requires that the cursor not be at the beginning of the list.
setPrev :: a -> State (ListZipper a) ()
setPrev y = modify $ \(ListZipper (_:ys) xs) -> ListZipper (y:ys) xs


-- | Moves the cursor to the beginning of the list.
rewind :: State (ListZipper a) ()
rewind = modify $ \(ListZipper ys xs) -> ListZipper [] $ foldl (flip (:)) xs ys
