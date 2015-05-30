
import Common.Numbers.Numbers (powMod)

main = print $ (sum (map (\x -> powMod x x modulo) [1 .. 1000])) `mod` modulo where 
    modulo = 10000000000 :: Integer

