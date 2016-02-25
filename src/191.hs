{-# LANGUAGE TupleSections #-}

import Data.Maybe (fromJust, isJust)
import Data.List (groupBy, sort)
import Data.Function (on)

data Attendance = A | L | O
type State = (Int, Int)

transtate :: State -> Attendance -> Maybe State
transtate (1, _) L = Nothing
transtate (_, 2) A = Nothing
transtate (0, _) L = Just (1, 0)
transtate (n, _) O = Just (n, 0)
transtate (n, k) A = Just (n, k + 1)

go :: [(State, Int)] -> [(State, Int)]
go vs = merge $ map fromJust $ filter isJust $ [ (, c) <$> transtate v a | (v, c) <- vs, a <- [A, L, O] ]
  where
    merge vs = map f $ groupBy ((==) `on` fst) $ sort vs
    f vs = (fst (vs!!0), sum $ map snd vs)

main = print $ sum $ map snd $ (iterate go [((0, 0), 1)]) !! 30
