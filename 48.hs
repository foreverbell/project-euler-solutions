
powmod :: Integer -> Integer -> Integer -> Integer
powmod a 0 m = 1
powmod a 1 m = a `mod` m
powmod a p m 
    | even p = (rec ^ 2) `mod` m
    | odd  p = ((rec ^ 2) * a) `mod` m
    where rec = powmod a (p `div` 2) m

main = print $ foldl (\s x -> (s + (powmod x x modulo)) `mod` modulo) 0 [1 .. 1000]
    where modulo = 10000000000
