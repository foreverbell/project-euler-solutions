{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

import qualified Common.Matrix.Matrix as M
import           Common.NumMod.MkNumMod

mkNumMod True 100000000
type Zn = Int100000000

solve :: Integer -> Zn
solve n = (((mat `M.power` p) `M.multiply` initial) M.! (1, 1)) - 1 
  where
    p = 987654321 :: Int
    mat = M.fromList 3 3 [0, 1, 0, 0, 0, 1, fromInteger $ -n, 0, 2^n]
    initial = M.fromList 3 1 [3, 2^n, 4^n]

main = print $ sum [ solve i | i <- [1 .. 30] ]
