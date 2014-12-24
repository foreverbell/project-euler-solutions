
mulMod m a b = (a * b) `mod` m

addMod m a b = (a + b) `mod` m

powMod m a p
    | p == 1 = a `mod` m
    | p == 0 = 1
    | even p = mulMod m half half
    | odd p = mulMod m a $ mulMod m half half
    where half = powMod m a (p `div` 2)

modulo = 10^10

main = print $ addMod modulo 1 $ mulMod modulo 28433 $ powMod modulo 2 7830457
