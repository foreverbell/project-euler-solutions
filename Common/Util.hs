module Common.Util (
    if',
    isqrt,
    submasks,
    combmasks
) where

import Data.Bits

{-# INLINABLE if' #-}
{-# INLINABLE isqrt #-}

if' :: Bool -> t -> t -> t
if' p a b = if p 
    then a 
    else b

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

submasks :: Int -> [Int]
submasks mask = 0 : takeWhile (/= 0) (iterate (\sub -> (sub - 1) .&. mask) mask)

combmasks :: Int -> Int -> [Int]
combmasks n k = takeWhile (< limit) $ iterate iter $ (1 `shiftL` k) - 1 where
    limit = 1 `shiftL` n
    iter comb = (((comb .&. (complement y)) `div` x) `shiftR` 1) .|. y where
        x = comb .&. (-comb)
        y = comb + x
