factorial :: Integer -> Integer
factorial x = product [1 .. x]

choose :: Integer -> Integer -> Integer
choose n k = (factorial n) `quot` (factorial k) `quot` (factorial (n - k))

main = do
    print (choose 40 20)
