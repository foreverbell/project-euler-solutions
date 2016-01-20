
main = print b where
    magic = [ (a,c) | a <- [1 .. 9], b <- [1 .. 9], c <- [a+1 .. 9], (10*a+b)*c == (10*b+c)*a || (10*b+a)*c == (10*c+b)*a ]
    (numerator, denominator) = unzip magic
    a' = product numerator
    b' = product denominator
    g = gcd a' b'
    a = a' `div` g
    b = b' `div` g

