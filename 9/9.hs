main = print (head tripleProduct)
    where tripleProduct = [ a*b*c | a <- [1 .. 1000], b <- [1 .. 1000 - a], c <- [1000 - a - b], a^2 + b^2 == c^2 ]
