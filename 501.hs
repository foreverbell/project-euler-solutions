
-- ~5minutes, extremely slow
-- required: data-memocombinators

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.MemoCombinators as Memo
import Control.Monad (forM_)
import Common.Primes (primesTo)

iroot x p = floor $ (fromIntegral x) ** (1/p)

n = 10^12 :: Int
crootN = iroot n 3 :: Int
sieveN = crootN^2 :: Int

primes = V.fromList $ primesTo sieveN 
primesCnt = V.length primes
primes' = V.create $ do
    was <- MV.replicate (sieveN + 1) (0 :: Int)
    forM_ [0 .. primesCnt - 1] $ \i -> do
        MV.write was (primes V.! i) 1
    forM_ [1 .. sieveN] $ \i -> do
        last <- MV.read was (i - 1)
        cur <- MV.read was i
        MV.write was i (cur + last)
    return was
nthPrime n = V.unsafeIndex primes (n - 1)

meissel 0 m = m
meissel i 0 = 0
meissel i m = Memo.memo2 (Memo.unsafeArrayRange (0, crootN)) (Memo.arrayRange (0, 5000)) meissel' i m where
    meissel' i m = a - b where
        a = meissel (i - 1) m
        b = meissel (i - 1) (m `div` p)
        p = nthPrime i

countP m = if (m <= sieveN)
    then primes' V.! m
    else Memo.bits countP' m where
        countP' m = cnt' + n*(u+1) + (u*u-u)`div`2 - 1 - cnt'' where
            n = countP croot
            u = (countP root) - n
            root = iroot m 2
            croot = iroot m 3
            cnt' = meissel n m
            cnt'' = sum $ map (\p -> countP (m `div` (nthPrime (n+p)))) [1 .. u]

main = primes `seq` print $ kind1 + kind2 + kind3 + kind4 where
    root4 = iroot n 4
    primes4 = zip [1 .. ] $ V.toList $ V.takeWhile (<= root4) $ primes
    kind1 = countP $ iroot n 7
    kind2 = sum $ map count primes4 where
        count (i, p) = countP (iroot (n `div` p) 3) - i
    kind3 = sum $ map count primes4 where
        count (i, p) = countP (n `div` (p^3)) - i
    kind4 = dfs 1 1 1 where
        dfs 1 x index = if (index == primesCnt || p*p*p > n) then 0 else (dfs 2 (x*p) (index + 1)) + (dfs 1 x (index + 1)) where
            p = nthPrime index
        dfs 2 x index = if (index == primesCnt || x*p*p > n) then 0 else (countP (n `div` (x*p))) - index + (dfs 2 x (index + 1)) where
            p = nthPrime index

