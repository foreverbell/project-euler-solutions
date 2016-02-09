import Text.Printf (printf)

solve :: Double -> Double
solve x = 2/9 * x**3 - 2/3 * log x + 7/9

main = printf "%.10f\n" $ solve (1/40)
