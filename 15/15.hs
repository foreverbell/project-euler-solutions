factorial :: Integer -> Integer
factorial x = product [1 .. x]

choose :: Integer -> Integer -> Integer
choose n k = (factorial n) `div` (factorial k) `div` (factorial (n - k))

main = print (choose 40 20)
