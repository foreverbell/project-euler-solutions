module Common.Numbers.EulerPhi (
  phi
, phiTo
) where

import           Common.Utils (if')
import           Common.Numbers.Primes (primes', countPrimeApprox)
import qualified Common.MonadRef as R
import           Control.Monad (when, forM_)
import           Control.Monad.Trans.Loop (iterateLoopT, exit) 
import           Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V

phi :: Int -> Int
phi n = loop 1 n primes' 
  where
    loop :: Int -> Int -> [Int] -> Int
    loop ret 1 _ = ret
    loop ret n (p:ps) = if n `rem` p /= 0
      then loop ret n ps
      else loop ret' n' ps 
        where
          (n', a) = divide 1 (n `quot` p) p
          ret' = ret * (p - 1) * a
          divide ph re p = if re `rem` p /= 0
              then (re, ph)
              else divide (ph * p) (re `quot` p) p
    loop _ _ [] = undefined

phiTo :: Int -> [Int]
phiTo n = tail $ V.toList $ V.create $ do
  pt <- R.new (0 :: Int)
  sieve <- MV.replicate (n + 1) True
  primes <- MV.replicate (countPrimeApprox n + 1) (0 :: Int)
  phi <- MV.replicate (n + 1) (1 :: Int)
  forM_ [2 .. n] $ \i -> do
    isPrime <- MV.unsafeRead sieve i
    when isPrime $ do
      pt' <- R.modify' pt (+ 1) 
      MV.unsafeWrite primes pt' i
      MV.unsafeWrite phi i (i - 1)
    phi' <- MV.unsafeRead phi i
    pt' <- R.read pt
    iterateLoopT 1 $ \j -> 
      if' (j > pt') exit $ do
        p' <- lift $ MV.unsafeRead primes j
        if' (p' * i > n) exit $ do
          lift $ MV.unsafeWrite sieve (p' * i) False
          lift $ MV.unsafeWrite phi (p' * i) (phi' * if' (i `rem` p' == 0) p' (p' - 1))
          if' (i `rem` p' == 0) exit $ return $ j + 1
  return phi
