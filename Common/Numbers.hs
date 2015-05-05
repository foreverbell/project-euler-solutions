module Common.Numbers (
    powMod,
    exgcd,
    inverse,
    inverse'
) where

{-# INLINABLE powMod #-}
{-# INLINABLE exgcd #-}
{-# INLINABLE inverse #-}
{-# INLINABLE inverse' #-}

powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod a p m = helper a p m 1 where
    helper a 0 m ret = ret
    helper a p m ret = if odd p
        then helper a' p' m (ret * a `mod` m)
        else helper a' p' m ret where
            a' = a * a `mod` m
            p' = p `div` 2

exgcd :: (Integral a) => a -> a -> (a, a, a)
exgcd a 0 = (a, 1, 0)
exgcd a b = (d, y, x - (a `div` b) * y) where
    (d, x, y) = exgcd b (a `mod` b)

-- p should be a prime.
inverse :: (Integral a) => a -> a -> a
inverse x p = if x' == 0 
    then undefined
    else powMod x' (toInteger (p - 2)) p
    where x' = x `mod` p

-- x and m should be co-prime.
-- this version is preferred.
inverse' :: (Integral a) => a -> a -> a 
inverse' x m = if d /= 1
    then undefined
    else a `mod` m
    where (d, a, b) = exgcd x m

