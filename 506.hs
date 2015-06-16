
import qualified Common.Matrix.Int as M

initial = [1, 2, 3, 4, 32, 123, 43, 2123, 432, 1234, 32123, 43212, 34321, 23432, 123432]
suffix  = [234321, 343212, 432123, 321234, 123432, 432123, 212343, 432123, 123432, 321234, 432123, 343212, 234321, 123432, 123432]

modulo = 123454321 :: Int
(_, _, (*.), (^.)) = M.createOps modulo

solve = (sum (map calculate [1 .. 15])) `mod` modulo where
    n = 10^14 :: Int
    calculate i = ((mat ^. c) *. init) M.! (1, 1) where
        c = (n `div` 15) + (if (i <= (n `mod` 15)) then 1 else 0)
        s = suffix!!(i - 1)
        r = initial!!(i - 1)
        mat = M.fromList 3 3 [1, 1, 0, 0, 10^6, s, 0, 0, 1]
        init = M.fromList 3 1 [0, r, 1]
        
main = print solve
