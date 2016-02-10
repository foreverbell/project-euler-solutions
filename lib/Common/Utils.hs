module Common.Utils (
  if'
, (?)
, isqrt
, modifyArray
, initArray
, submasks
, combmasks
) where

import Control.Monad (forM_)
import Data.Array.MArray (MArray, newArray, readArray, writeArray)
import Data.Bits (shiftL, shiftR, complement, (.&.), (.|.))
import Data.Ix (Ix)

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

modifyArray :: (MArray a e m, Ix i) => a i e -> (e -> e) -> i -> m ()
{-# INLINABLE modifyArray #-}
modifyArray a f i = readArray a i >>= writeArray a i . f

initArray :: (MArray a e m, Ix i) => (i, i) -> e -> [(i, e)] -> m (a i e)
initArray (l, u) initValue setValues = do
  a <- newArray (l, u) initValue
  forM_ setValues $ \(i, e) -> writeArray a i e 
  return a

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
