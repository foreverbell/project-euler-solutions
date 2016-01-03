
sumOfDivisors n = sum $ filter (\x -> n `mod` x == 0) [1 .. (n - 1)]

amicable = [ x | x <- [1 .. 10000], y <- [sumOfDivisors x], y /= x, sumOfDivisors y == x ]

main = print $ sum amicable
