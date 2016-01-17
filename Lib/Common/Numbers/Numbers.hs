module Common.Numbers.Numbers (
  factorial,
  binomial,
  powMod,
  fastpow,
  exgcd,
  inverse,
  inverse',
  inverseToM,
  inverseTo,
  crt2,
  crt
) where

import Data.Bits (Bits, (.&.), shiftR)
import qualified Data.Array as A
import Control.Monad.Identity

{-# INLINABLE factorial #-}
{-# INLINABLE binomial #-}
{-# INLINABLE powMod #-}
{-# INLINABLE fastpow #-}
{-# INLINABLE exgcd #-}
{-# INLINABLE inverse #-}
{-# INLINABLE inverse' #-}
{-# INLINABLE inverseToM #-}
{-# INLINABLE inverseTo #-}
{-# INLINABLE crt2 #-}
{-# INLINABLE crt #-}

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

binomial :: (Integral a) => a -> a -> a
binomial a b = if a < b
  then 0
  else product [b + 1 .. a] `quot` product [1 .. (a - b)]

powMod :: (Integral a, Bits b, Integral b) => a -> b -> a -> a
powMod a p m = helper a p m 1
  where
    helper _ 0 _ ret = ret
    helper a p m ret = if (p .&. 1) == 1
        then helper a' p' m (ret * a `rem` m)
        else helper a' p' m ret 
          where
            a' = a * a `rem` m
            p' = p `shiftR` 1

fastpow :: (Num a, Bits b, Integral b) => a -> b -> a
fastpow a p = helper a p 1 
  where
    helper _ 0 ret = ret
    helper a p ret = if (p .&. 1) == 1
        then helper a' p' (ret * a)
        else helper a' p' ret 
          where
            a' = a * a
            p' = p `shiftR` 1

exgcd :: (Integral a) => a -> a -> (a, a, a)
exgcd a 0 = (a, 1, 0)
exgcd a b = (d, y, x - (a `quot` b) * y) 
  where
    (d, x, y) = exgcd b (a `rem` b)

-- p should be a prime.
inverse :: (Integral a) => a -> a -> a
inverse x p = if x' == 0 
  then undefined
  else powMod x' (toInteger (p - 2)) p
  where 
    x' = x `rem` p

-- x and m should be co-prime.
-- this version is preferred.
inverse' :: (Integral a) => a -> a -> a 
inverse' x m = if d /= 1
  then undefined
  else a `rem` m
  where 
    (d, a, _) = exgcd x m

inverseToM :: (Monad m, Integral a) => Int -> a -> [m a]
inverseToM n m = A.elems cache 
  where
    cache = A.listArray (0, n) $ fail "undefined" : return 1 : map inv [2 .. n]
    inv x = do
      let (q, r) = m `quotRem` fromIntegral x
      y <- cache A.! fromIntegral r
      return $ y * (m - q) `rem` m

inverseTo :: (Integral a) => Int -> a -> [a]
inverseTo n m = map runIdentity $ inverseToM n m 

crt2 :: (Integral a) => (a, a) -> (a, a) -> a
crt2 (p1, r1) (p2, r2) = (a + b) `rem` n
  where
    n = p1 * p2
    a = inverse' p2 p1 * p2 `rem` n * r1 `mod` n
    b = inverse' p1 p2 * p1 `rem` n * r2 `mod` n

crt :: (Integral a) => [(a, a)] -> a
crt = loop 1 1 
  where
    loop _ res [] = res
    loop pp res ((p, r):rest) = loop (pp * p) (crt2 (pp, res) (p, r)) rest

