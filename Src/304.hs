{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

import qualified Common.Numbers.Primes as P
import qualified Common.Matrix.Matrix as M
import           Common.NumMod.MkNumMod
import           Data.List (foldl')

mkNumMod True 1234567891011
type Zn = Int1234567891011

primes = take 100000 $ filter P.testPrime [10^14 .. ] :: [Int]

fibonacci :: Int -> Zn
fibonacci n = (base `M.power` n) M.! (1, 2) 
  where
    base = M.fromList 2 2 [1, 1, 1, 0]

main = print $ foldl' (+) 0 $ map fibonacci primes
