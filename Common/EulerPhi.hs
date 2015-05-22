module Common.EulerPhi (
    phi,
    phiTo
) where

import Common.Util (if')
import Common.Primes (primes', countPrimeApprox)
import qualified Common.Ref as R
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (when, forM_)
import Control.Monad.Trans.Loop (iterateLoopT, exit) -- require control-monad-loops
import Control.Monad.Trans.Class (lift)

phi :: Int -> Int
phi n = loop 1 n primes' where
    loop :: Int -> Int -> [Int] -> Int
    loop ret 1 _ = ret
    loop ret n (p:ps) = if n `rem` p /= 0
        then loop ret n ps
        else loop ret' n' ps where
            (n', a) = divide 1 (n `quot` p) p
            ret' = ret * (p - 1) * a
            divide ph re p = if re `rem` p /= 0
                then (re, ph)
                else divide (ph * p) (re `quot` p) p

phiTo :: Int -> [Int]
phiTo n = elems $ runSTUArray $ do
    pt <- R.new (0 :: Int)
    sieve <- newArray (2, n) True :: ST s (STUArray s Int Bool)
    primes <- newArray (1, countPrimeApprox n) 0 :: ST s (STUArray s Int Int)
    phi <- newArray (1, n) 1
    forM_ [2 .. n] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            R.modify pt (+ 1) 
            pt' <- R.read pt
            writeArray primes pt' i
            writeArray phi i (i - 1)
        phi' <- readArray phi i
        pt' <- R.read pt
        iterateLoopT 1 $ \j -> do
            if' (j > pt') exit $ do
                p' <- lift $ readArray primes j
                if' (p' * i > n) exit $ do
                    lift $ writeArray sieve (p' * i) False
                    lift $ writeArray phi (p' * i) (phi' * (if' (i `rem` p' == 0) p' (p' - 1)))
                    if' (i `rem` p' == 0) exit $ return $ j + 1
    return phi
