-- dependencies: matrix

import qualified Data.Matrix as M

initial = [1, 2, 3, 4, 32, 123, 43, 2123, 432, 1234, 32123, 43212, 34321, 23432, 123432]
suffix = [234321, 343212, 432123, 321234, 123432, 432123, 212343, 432123, 123432, 321234,432123, 343212, 234321, 123432, 123432]

modulo = 123454321 :: Int

data IntM = IntM Int

valueIntM (IntM x) = x `mod` modulo

instance Num IntM where
    a + b = IntM $ ((valueIntM a) + (valueIntM b)) `mod` modulo where
    a * b = IntM $ ((valueIntM a) * (valueIntM b)) `mod` modulo where
    negate a = let a' = valueIntM a in IntM $ (modulo - a') `mod` modulo
    abs a = undefined
    signum a = undefined
    fromInteger x = IntM (fromInteger x)

powMat :: (Num a) => M.Matrix a -> Int -> M.Matrix a
powMat a p = if (n == m) 
    then helper a p (M.identity n) 
    else undefined
    where
        n = M.ncols a
        m = M.nrows a
        helper a 0 r = r
        helper a p r = helper (M.multStd a a) (p `div` 2) r' where
            r' = if (odd p) then M.multStd r a else r

solve = valueIntM $ sum $ map calculate [1 .. 15] where
    n = 10^14 :: Int
    calculate i = (M.multStd (powMat mat c) init) M.! (1, 1) where
        c = (n `div` 15) + (if (i <= (n `mod` 15)) then 1 else 0)
        s = suffix!!(i - 1)
        r = initial!!(i - 1)
        mat = M.fromList 3 3 $ map IntM [1, 1, 0, 0, 10^6, s, 0, 0, 1]
        init = M.fromList 3 1 $ map IntM [0, r, 1]
        
main = print solve
