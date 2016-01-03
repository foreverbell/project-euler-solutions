
import qualified Common.Matrix.Int as M
import qualified Common.Numbers.Numbers as N

modulo = 1307674368000

add :: Int -> Int -> Int -> Int
add m a b = (a + b) `rem` m

mul :: Int -> Int -> Int -> Int
mul m a b = fromInteger $ (toInteger a) * (toInteger b) `rem` (toInteger m)

fibonacci n m = head $ (M.fromList 2 2 [1, 1, 1, 0]) `power` (n - 1) where
    head m = m M.! (1, 1)
    power = M.power (add m) (mul m)

fn :: Int -> Int -> Int
fn n x = nume `div` deno where
    deno = x * x + x - 1
    m = modulo * deno
    nume = ((mul m f1 (power x (n + 2) m)) + (mul m f2 (power x (n + 1) m)) - x) `mod` m
    f1 = fibonacci n m
    f2 = fibonacci (n + 1) m
    power a p m = fromInteger $ N.powMod (toInteger a) (toInteger p) (toInteger m)

solve = (sum (map (fn (10^15)) [0 .. 100])) `mod` modulo

main = print solve
