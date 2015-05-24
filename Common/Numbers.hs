module Common.Numbers (
    factorial,
    binomial,
    powMod,
    exgcd,
    inverse,
    inverse',
    inverseToM,
    inverseTo
) where

import Data.Bits (Bits, (.&.), shiftR)
import qualified Data.Array as A
import Control.Monad (fail)
import Data.Maybe (fromJust)

{-# INLINABLE factorial #-}
{-# INLINABLE binomial #-}
{-# INLINABLE powMod #-}
{-# INLINABLE exgcd #-}
{-# INLINABLE inverse #-}
{-# INLINABLE inverse' #-}
{-# INLINABLE inverseToM #-}
{-# INLINABLE inverseTo #-}

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

binomial :: (Integral a) => a -> a -> a
binomial a b = if a < b
    then 0
    else (product [b + 1 .. a]) `quot` (product [1 .. (a - b)]) 

powMod :: (Integral a, Bits b, Integral b) => a -> b -> a -> a
powMod a p m = helper a p m 1 where
    helper a 0 m ret = ret
    helper a p m ret = if ((p .&. 1) == 1)
        then helper a' p' m (ret * a `rem` m)
        else helper a' p' m ret where
            a' = a * a `rem` m
            p' = p `shiftR` 1

exgcd :: (Integral a) => a -> a -> (a, a, a)
exgcd a 0 = (a, 1, 0)
exgcd a b = (d, y, x - (a `quot` b) * y) where
    (d, x, y) = exgcd b (a `rem` b)

-- p should be a prime.
inverse :: (Integral a) => a -> a -> a
inverse x p = if x' == 0 
    then undefined
    else powMod x' (toInteger (p - 2)) p
    where x' = x `rem` p

-- x and m should be co-prime.
-- this version is preferred.
inverse' :: (Integral a) => a -> a -> a 
inverse' x m = if d /= 1
    then undefined
    else a `rem` m
    where (d, a, b) = exgcd x m

inverseToM :: (Monad m, Integral a) => Int -> a -> [m a]
inverseToM n m = A.elems cache where
    cache = A.listArray (0, n) $ (fail "undefined") : (return 1) : (map inv [2 .. (fromIntegral n)]) where
        inv x = do
            y <- cache A.! (fromIntegral (m `rem` x))
            return $ y * (m - m `quot` x) `rem` m

inverseTo :: (Integral a) => Int -> a -> [a]
inverseTo n m = map fromJust $ inverseToM n m 
