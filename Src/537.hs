{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

-- TODO: needs profiling to improve performance. 
{-
  real    4m6.950s
  user    4m5.785s
  sys     0m1.039s
-}


import qualified Common.Polynomial.Polynomial as P
import           Common.NumMod.MkNumMod
import           Common.Numbers.Primes (primes)
import           Common.Numbers.Numbers (fastpow)

mkNumMod True 1004535809
type Zn = Int1004535809

newtype Poly = Poly (P.Polynomial Zn)

instance Num Poly where
  Poly p1 + Poly p2 = Poly $ p1 + p2
  Poly p1 - Poly p2 = Poly $ p1 - p2
  Poly p1 * Poly p2 = Poly $ P.fromList $ take 20001 . P.toList $ p1 * p2
  fromInteger = Poly . fromInteger

poly :: Poly
poly = Poly . P.fromList $ map toZn $ 1 : zipWith (-) (tail primeList) primeList
  where
    primeList = take 20001 primes
    toZn = fromInteger . toInteger

main = print $ p P.! 20000
  where Poly p = poly `fastpow` (20000 :: Int)
