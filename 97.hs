
import Common.Numbers (powMod)

main = print $ 1 + (28433 * (powMod 2 (7830457 :: Int) modulo) `mod` modulo) where
    modulo = 10^10 :: Integer
