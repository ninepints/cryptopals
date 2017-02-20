module Data.Zipper.List (
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


data ListZipper a = ListZipper {
    getConsumed :: [a],
    getRemaining :: [a]
} deriving (Show)

instance Functor ListZipper where
    fmap f (ListZipper ys xs) = ListZipper (map f ys) (map f xs)


next :: State (ListZipper a) (Maybe a)
next = state $ \zipper -> case zipper of
    ListZipper ys (x:xs) -> (Just x, ListZipper (x:ys) xs)
    _ -> (Nothing, zipper)

prev :: State (ListZipper a) (Maybe a)
prev = state $ \zipper -> case zipper of
    ListZipper (y:ys) xs -> (Just y, ListZipper ys (y:xs))
    _ -> (Nothing, zipper)


hasNext :: State (ListZipper a) Bool
hasNext = gets $ not . null . getRemaining

hasPrev :: State (ListZipper a) Bool
hasPrev = gets $ not . null . getConsumed

getNext :: State (ListZipper a) (Maybe a)
getNext = gets $ listToMaybe . getRemaining

getPrev :: State (ListZipper a) (Maybe a)
getPrev = gets $ listToMaybe . getConsumed

setNext :: a -> State (ListZipper a) ()
setNext x = modify $ \(ListZipper ys (_:xs)) -> ListZipper ys (x:xs)

setPrev :: a -> State (ListZipper a) ()
setPrev y = modify $ \(ListZipper (_:ys) xs) -> ListZipper (y:ys) xs


rewind :: State (ListZipper a) ()
rewind = modify $ \(ListZipper ys xs) -> ListZipper [] $ foldl (flip (:)) xs ys
