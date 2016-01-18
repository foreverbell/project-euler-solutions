module Common.Util (
  if'
, (?)
, isqrt
, submasks
, combmasks
) where

import Data.Bits

if' :: Bool -> t -> t -> t
{-# INLINE if' #-}
if' p a b = if p 
  then a 
  else b

infixl 2 ?
{-# INLINE (?) #-}
p ? t = if' p (const t) id

isqrt :: (Integral a) => a -> a
{-# INLINABLE isqrt #-}
isqrt = floor . sqrt . fromIntegral

submasks :: Int -> [Int]
{-# INLINE submasks #-}
submasks mask = 0 : takeWhile (/= 0) (iterate (\sub -> (sub - 1) .&. mask) mask)

combmasks :: Int -> Int -> [Int]
{-# INLINE combmasks #-}
combmasks n k = takeWhile (< limit) $ iterate iter $ (1 `shiftL` k) - 1 
  where
    limit = 1 `shiftL` n
    iter comb = (((comb .&. complement y) `div` x) `shiftR` 1) .|. y 
      where
        x = comb .&. (-comb)
        y = comb + x
