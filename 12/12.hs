numOfDivisors x = length (filter (\d -> x `mod` d == 0) [1 .. x])
numOfTriDivisors x 
    | odd x  = (numOfDivisors x) * (numOfDivisors ((x + 1) `quot` 2))
    | even x = (numOfDivisors (x `quot` 2)) * (numOfDivisors (x + 1))  

main = do
    print ((n * (n + 1)) `quot` 2)
    where n = head (dropWhile (\x -> (numOfTriDivisors x) <= 500) [1 .. ])
