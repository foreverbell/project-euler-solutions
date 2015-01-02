main = print diff where 
    diff = sum1 - sum2
    sum1 = (sum [1 .. 100]) ^ 2
    sum2 = sum (map (^2) [1 .. 100])
