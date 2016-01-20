{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

import qualified Common.Matrix.Matrix as M
import           Common.NumMod.MkNumMod

mkNumMod True 123454321
type Zn = Int123454321

initial = [1, 2, 3, 4, 32, 123, 43, 2123, 432, 1234, 32123, 43212, 34321, 23432, 123432]
suffix  = [234321, 343212, 432123, 321234, 123432, 432123, 212343, 432123, 123432, 321234, 432123, 343212, 234321, 123432, 123432]

solve :: Zn
solve = sum [ calculate i | i <- [1 .. 15] ]
  where
    n = 10^14 :: Int
    (q, r) = n `quotRem` 15
    calculate i = ((matrix `M.power` c) `M.multiply` init) M.! (1, 1)
      where
        c = q + (if i <= r then 1 else 0)
        matrix = M.fromList 3 3 [1, 1, 0, 0, 10^6, suffix !! (i - 1), 0, 0, 1]
        init = M.fromList 3 1 [0, initial !! (i - 1), 1]
       
main = print solve
