import qualified Common.Matrix.Matrix as M
import qualified Common.Numbers.Numbers as N
import           Common.NumMod.NumMod

modulo = 1307674368000 :: Int

fibonacci :: Int -> Int -> IntMod
fibonacci n m = head $ M.fromList 2 2 (map (fromInt m) [1, 1, 1, 0]) `M.power` (n-1)
  where
    head = \m -> m M.! (1, 1)

f :: Int -> Int -> IntMod
f n x = fromInt modulo $ b `div` a 
  where
    m = modulo * a :: Int
    x' = fromInt m x :: IntMod
    a = x * x + x - 1 :: Int
    b = toInt $ f1 * (x' `N.fastpow` (n+2)) + f2 * (x' `N.fastpow` (n+1)) - x'
    f1 = fibonacci n m
    f2 = fibonacci (n + 1) m

main = print $ sum [ f (10^15) i | i <- [1 .. 100] ]
