import Common.List (nub', minus)

sumOfDivisors n = sum $ filter (\x -> n `mod` x == 0) [1 .. (n - 1)]

abundant = [ x | x <- [1 .. 28123], sumOfDivisors x > x ]
magicNumber' = nub' [ x + y | x <- abundant, y <- abundant, x + y <= 28123 ]
magicNumber  = [1 .. 28123] `minus` magicNumber'

main = print $ sum magicNumber

