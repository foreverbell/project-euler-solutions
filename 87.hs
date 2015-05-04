import Data.List (group, sort)

limit = 50000000

primesTo m = eratos [2 .. m]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

primes = primesTo 7072

numbers :: [Int]
numbers = do
    x <- primes
    y <- takeWhile (\n -> n^3+x^2 <= limit) primes
    z <- takeWhile (\n -> x^2+y^3+n^4 <= limit) primes
    return (x^2+y^3+z^4)

nubLength = length . group . sort

main = print $ nubLength numbers
