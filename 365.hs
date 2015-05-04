
import Common.MapReduce (mapReduce)
import Common.Primes (primesTo)
import Common.Numbers (inverse')
import Data.List (tails)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import Control.Monad

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

crt3 :: [(Int, Int)] -> Int
crt3 xs = ((a * m1) + (b * m2) + (c * m3)) `mod` n where
    (p1, m1) = xs!!0
    (p2, m2) = xs!!1
    (p3, m3) = xs!!2
    n = p1 * p2 * p3
    a = ((inverse' (p2 * p3) p1) * (p2 * p3)) `mod` n
    b = ((inverse' (p1 * p3) p2) * (p1 * p3)) `mod` n
    c = ((inverse' (p1 * p2) p3) * (p1 * p2)) `mod` n

factorialInverse p = runSTUArray $ do
    inv <- newListArray (0, p - 1) (take p (1:1:(repeat 0)))
    forM_ [2 .. p - 1] $ \i -> do
        x <- readArray inv (p `mod` i)
        writeArray inv i $ (x * (p - (p `div` i))) `mod` p
    forM_ [2 .. p - 1] $ \i -> do
        x <- readArray inv (i - 1)
        y <- readArray inv i
        writeArray inv i $ (x * y) `mod` p
    return inv

factorial p = runSTUArray $ do
    ret <- newListArray (0, p - 1) (take p (1:(repeat 0)))
    forM_ [1 .. p - 1] $ \i -> do
        x <- readArray ret (i - 1)
        writeArray ret i $ (x * i) `mod` p
    return ret

-- Lucas theorem
lucas p n m = productMod (zipWith (lucas' p) (expand n p) (expand m p)) where
    expand 0 p = []
    expand n p = (n `mod` p) : expand (n `div` p) p
    xs = factorial p
    ys = factorialInverse p
    lucas' p n m = case compare n m of
        LT -> 0
        _  -> ((xs!n) * (ys!m) * (ys!(n-m))) `mod` p
    productMod xs = foldl helper 1 xs where
        helper accum x = (accum * x) `mod` p

solve = mapReduce 23 solve sum [0 .. (length primes) - 1] where
    primes = dropWhile (<= 1000) $ primesTo 5000
    binomials = zip primes $ map (\p -> lucas p (10^18) (10^9)) primes
    solve i = sum $ map (\r -> crt3 (h:r)) rs where
        h = binomials !! i
        rs = combinations 2 $ drop (i + 1) binomials

main = print solve
