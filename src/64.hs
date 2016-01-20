import qualified Data.Map as M
import Common.Utils (isqrt)

period :: Int -> Int -> Int -> Int -> Int -> M.Map (Int, Int) Int -> Int
period n root a b index map = 
    case M.lookup (a, b) map of 
        Just index2 -> index - index2
        otherwise -> period n root a' b' (index+1) (M.insert (a, b) index map) 
            where
                a' = (n - b * b) `div` a
                x = (root + b) `div` a'
                b' = x * a' - b

findPeriod n = period n root 1 root 0 M.empty where
    root = isqrt n

main = print $ length $ filter (odd . findPeriod) irrational where
    isSquared x = (isqrt x)^2 == x
    irrational = filter (not . isSquared) [1 .. 10000]

-- analysis:
-- denote the state (a,b) as a / (sqrt n - b), we can prove that a|n-b^2 by induction.
-- then by recursion, a'=(n-b^2)/a, b'=x*a'-b, where x=(root+b)/a
