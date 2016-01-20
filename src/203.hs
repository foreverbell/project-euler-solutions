import Common.Numbers.Primes (primesTo)
import Common.List (nub')
import Common.Utils (isqrt)

n = 50

binomial :: Int -> Int -> Int
binomial n k = fromIntegral $ a `div` b
  where
    a = product [n' - k' + 1 .. n']
    b = product [1 .. k']
    n' = fromIntegral n :: Integer
    k' = fromIntegral (min k (n - k)) :: Integer

primes = primesTo upto
  where upto = isqrt $ binomial n (n `div` 2)

squarefree n = go n primes
  where
    go _ [] = True
    go 1 _ = True
    go n (p:ps) | n `mod` p == 0 = if n `mod` (p^2) == 0 then False else go (n `div` p) ps
                | otherwise = go n ps 

numbers = nub' $ concatMap (\i -> [ binomial i k | k <- [0 .. i] ]) [1 .. n]

main = print $ sum $ filter squarefree numbers
