import Text.Printf (printf)

v = 20.0 :: Double
g = 9.81 :: Double
y_min = -100.0 :: Double
y_max = v * v / 2 / g

f y = pi * v4 / g2 * y - pi * v2 / g * y * y
  where
    v2 = v * v
    v4 = v2 * v2
    g2 = g * g

main = printf "%.4f\n" $ f y_max - f y_min
