
type Matrix2 = (Int, Int, Int, Int)

multiplyMat (a, b, c, d) (a', b', c', d') m = ((a * a' + b * c') `mod` m, (a * b' + b * d') `mod` m, (c * a' + d * c') `mod` m, (c * b' + d * d') `mod` m)

powerMat mat 0 m = (1, 0, 0, 1)
powerMat mat 1 m = mat
powerMat mat p m = if (even p) 
    then multiplyMat half half m
    else multiplyMat (multiplyMat half half m) mat m
    where half = powerMat mat (p `div` 2) m

fibonacci n m = m11 $ powerMat (1, 1, 1, 0) (n - 1) m
    where m11 (a, _, _, _) = a

powerMod a 0 m = 1
powerMod a 1 m = a `mod` m
powerMod a p m = case even p of
    True  -> multiplyMod half half m
    False -> multiplyMod (multiplyMod half half m) a m 
    where half = powerMod a (p `div` 2) m
          multiplyMod a b m = (a * b) `mod` m

modulo = 1307674368000

fn :: Integer -> Integer -> Integer
fn n x = nume `div` deno where
    deno = x * x + x - 1
    m = modulo * deno
    nume = (f1 * (powerMod x (n + 2) m) + f2 * (powerMod x (n + 1) m) - x) `mod` m
    f1 = fibonacci n m
    f2 = fibonacci (n + 1) m

solve = (sum (map (fn (10^15)) [0 .. 100])) `mod` modulo

main = print solve
