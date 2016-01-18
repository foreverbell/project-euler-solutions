{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

-- TODO: ~15 minutes, needs profiling

import           Control.Monad (forM_, when)
import           Control.Monad.ST (runST, ST)
import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Common.DataStructure.Fenwick as F
import           Common.NumMod.MkNumMod

import           Prelude hiding (read)

mkNumMod True 1000000000000000000
type Zn = Int1000000000000000000

(!) = (V.!)

read :: (PrimMonad m, MV.Unbox a) => MV.MVector (PrimState m) a -> Int -> m a
read = MV.unsafeRead

write :: (PrimMonad m, MV.Unbox a) => MV.MVector (PrimState m) a -> Int -> a -> m () 
write = MV.unsafeWrite

eratos :: Int -> V.Vector Int
eratos n = V.create $ do
  isPrime <- MV.replicate (n + 1) True
  minPrimePart <- V.thaw $ V.fromList [0 .. n] 
  sumOfDivisors <- MV.replicate (n + 1) 2
  write sumOfDivisors 1 1
  forM_ [2 .. n] $ \i -> do
    prime <- read isPrime i
    when prime $ 
      forM_ [i^2, i^2 + i .. n] $ \j -> do
        prime' <- read isPrime j
        when prime' $ do
          write isPrime j False
          let q = j `div` i
          minPrime' <- read minPrimePart q
          if minPrime' `mod` i == 0
            then write minPrimePart j (minPrime' * i)
            else write minPrimePart j i
          d1 <- read minPrimePart j
          when (d1 == j) $ do
            s <- read sumOfDivisors q
            write sumOfDivisors j (s + 1)
  forM_ [2 .. n] $ \i -> do
    d1 <- read minPrimePart i
    let d2 = i `div` d1
    when (d1 /= i) $ do
      s1 <- read sumOfDivisors d1
      s2 <- read sumOfDivisors d2
      write sumOfDivisors i (s1 * s2)
  return sumOfDivisors

buildDT :: Int -> V.Vector Int
buildDT n = V.fromList $ map f [0 .. n]
  where
    d = eratos (n + 1)
    f 0 = 0
    f i | odd i = (d!i) * (d!((i + 1) `div` 2))
        | otherwise = (d!(i `div` 2)) * (d!(i + 1))

solve :: Int -> Zn
solve n = runST $ do
  let dT = buildDT n
  let maxd = V.maximum dT
  dp1 <- F.make maxd :: ST s (F.Fenwick (ST s) Zn)
  dp2 <- F.make maxd
  dp3 <- F.make maxd
  V.forM_ (V.drop 1 dT) $ \d -> do
    F.modify dp1 (+) d 1
    F.askLR dp1 (+) (d + 1) maxd >>= F.modify dp2 (+) d
    F.askLR dp2 (+) (d + 1) maxd >>= F.modify dp3 (+) d
  F.ask dp3 (+) maxd

main :: IO ()
main = print $ solve 60000000 
