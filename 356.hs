
import qualified Common.Matrix.Int as M

modulo = 10^8 :: Int
(_, _, (*.), (^.)) = M.createOps modulo

solve n = (((mat ^. (987654321 :: Int)) *. initial) M.! (1, 1)) - 1 where
    mat = M.fromList 3 3 [0, 1, 0, 0, 0, 1, -n, 0, 2^n]
    initial = M.fromList 3 1 [3, 2^n, (4^n) `rem` modulo]

main = print $ (sum [ solve i | i <- [1 .. 30] ]) `mod` modulo
