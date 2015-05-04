
-- ~5minutes, extremely slow
-- required: data-memocombinators

import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad (forM_, when)
import Data.List (tails, foldl', find)
import Data.Maybe (fromJust)
import Control.Parallel (pseq, par)
import qualified Data.MemoCombinators as Memo

iRoot x p = floor $ (fromIntegral x) ** (1/p)

primesTo m = zip [2 .. m] $ drop 2 $ V.toList $ V.create $ do
    sieve <- MV.replicate (m + 1) True
    let root = (floor . sqrt . fromIntegral) m
    forM_ [2 .. root] $ \i -> do
        isPrime <- MV.read sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                MV.write sieve j False
    return sieve
        
n = 10^12 :: Int
crootN = iRoot n 3 :: Int
sieveN = crootN^2 :: Int

primesN = map fst $ filter snd $ primesTo sieveN
primesLimit = length primesN
primesNA = V.fromList primesN 
nthPrime n = V.unsafeIndex primesNA (n - 1)

meissel = Memo.memo2 (Memo.unsafeArrayRange (0, crootN)) (Memo.arrayRange (0, 5000)) meissel' where
    meissel' 0 m = m
    meissel' i 0 = 0
    meissel' i m = a `par` b `pseq` a - b where
        a = meissel (i - 1) m
        b = meissel (i - 1) (m `div` p)
        p = nthPrime i

countP = Memo.bits countP' where
    countP' 0 = 0
    countP' 1 = 0
    countP' m = if m < sieveN
        then binSearch 1 primesLimit m
        else cnt' + n*(u+1) + (u*u-u)`div`2 - 1 - cnt'' where
            binSearch l r m = if l == r then l
                else let mid = (l + r) `div` 2 + 1 in
                    if ((nthPrime mid) <= m)
                        then binSearch mid r m
                        else binSearch 1 (mid - 1) m
            n = countP croot
            u = (countP root) - n
            root = iRoot m 2
            croot = iRoot m 3
            cnt' = meissel n m
            cnt'' = sum $ map (\p -> countP (m `div` (nthPrime (n+p)))) [1 .. u]

countPLR l r = if (l > r)
    then 0 
    else (countP r) - (countP (l - 1))

main = primesN `seq` primesNA `seq` print $ kind1 + kind2 + kind3 + kind4 where
    kind1 = countP $ iRoot n 7
    kind2 = sum $ map count $ takeWhile (<= (iRoot n 4)) primesN where
        count p = countPLR (p + 1) $ iRoot (n `div` p) 3
    kind3 = sum $ map count $ takeWhile (<= (iRoot n 4)) primesN where
        count p = countPLR (p + 1) $ n `div` (p^3)
    kind4 = dfs 1 1 primesN where
        dfs 1 x (p:ps) = if (p*p*p > n) then 0 else (dfs 2 (x*p) ps) + (dfs 1 x ps)
        dfs 2 x (p:ps) = if (x*p*p > n) then 0 else (countPLR (p+1) (n `div` (x*p))) + (dfs 2 x ps)
        dfs _ _ [] = 0

