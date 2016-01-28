{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

import qualified Common.Matrix.Matrix as M
import           Common.NumMod.MkNumMod

mkNumMod True 1000000000
type Zn = Int1000000000

solve :: Int -> Zn
solve n = sum $ M.toList $ (matrix `M.power` (n - 1)) `M.multiply` from
  where
    matrix = M.fromLists [ [1, 1, 1, 1, 1, 1]
                         , [6, 1, 1, 1, 1, 1]
                         , [0, 5, 1, 1, 1, 1]
                         , [0, 0, 4, 1, 1, 1]
                         , [0, 0, 0, 3, 1, 1]
                         , [0, 0, 0, 0, 2, 1]
                         ] 
    from = M.fromList 6 1 [7, 0, 0, 0, 0, 0]

main = print $ solve (10^12)
