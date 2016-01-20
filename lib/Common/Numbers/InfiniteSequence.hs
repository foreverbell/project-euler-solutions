module Common.Numbers.InfiniteSequence (
  prime
, fibnoacci
) where

import qualified Common.Numbers.Primes as P

prime :: Integral a => [a]
prime = P.primes

fibnoacci :: Integral a => [a]
fibnoacci = 0 : 1 : zipWith (+) fibnoacci (tail fibnoacci)
