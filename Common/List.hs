module Common.List (
    maximumBy',
    minimumBy'
) where

import Data.List (foldl1')

{-# INLINABLE maximumBy' #-}
{-# INLINABLE minimumBy' #-}

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp [] = undefined
maximumBy' cmp xs = foldl1' helper xs where
    helper a b = case cmp a b of
        LT -> b
        _ -> a

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp [] = undefined
minimumBy' cmp xs = foldl1' helper xs where
    helper a b = case cmp a b of
        GT -> b
        _ -> a

