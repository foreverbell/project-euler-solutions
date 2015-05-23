module Common.List (
    rotate,
    minus,
    intersect,
    nub',
    unique,
    maximumBy',
    minimumBy',
    maximum',
    minimum'
) where

import Data.List (foldl1', group)
import qualified Data.Set as S

{-# INLINABLE rotate #-}
{-# INLINABLE minus #-}
{-# INLINABLE intersect #-}
{-# INLINABLE nub' #-}
{-# INLINABLE unique #-}
{-# INLINABLE maximumBy' #-}
{-# INLINABLE minimumBy' #-}
{-# INLINABLE maximum' #-}
{-# INLINABLE minimum' #-}

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

-- set differeance (assert already sorted)
minus :: (Ord a) => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus xs'@(x:xs) ys'@(y:ys) = case (compare x y) of
    LT -> x : xs `minus` ys'
    EQ -> xs `minus` ys
    GT -> xs' `minus` ys

-- set intersection (assert already sorted)
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect xs'@(x:xs) ys'@(y:ys) = case (compare x y) of
    EQ -> x : xs `intersect` ys
    LT -> xs `intersect` ys'
    GT -> xs' `intersect` ys

nub' :: (Ord a) => [a] -> [a]
nub' = S.toList . S.fromList

-- test if one list has a unique element
unique :: (Eq a) => [a] -> Bool
unique xs = 1 == (length (group xs))

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ [] = undefined
maximumBy' cmp xs = foldl1' helper xs where
    helper a b = case cmp a b of
        LT -> b
        _ -> a

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' _ [] = undefined
minimumBy' cmp xs = foldl1' helper xs where
    helper a b = case cmp a b of
        GT -> b
        _ -> a

maximum' :: Ord a => [a] -> a
maximum' = foldl1' max

minimum' :: Ord a => [a] -> a
minimum' = foldl1' min
