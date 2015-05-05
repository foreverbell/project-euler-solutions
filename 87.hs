import Common.Primes (primesTo)
import Common.List (nub')

limit = 50000000
primes = primesTo 7072

numbers :: [Int]
numbers = do
    x <- primes
    y <- takeWhile (\n -> n^3+x^2 <= limit) primes
    z <- takeWhile (\n -> x^2+y^3+n^4 <= limit) primes
    return (x^2+y^3+z^4)

main = print $ length $ nub' numbers
