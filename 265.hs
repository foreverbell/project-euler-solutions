import Data.Bits (shiftL, shiftR, (.&.))
import Data.List (sort)

n = 5 :: Int

check :: Int -> Bool
check x = sum xs == sum0 && sort xs == [0 .. mask]
  where 
    xs = map (\d -> (shift d) .&. mask) [-n + 1 .. 2^n - n]
    sum0 = sum [0 .. mask]
    shift d | d < 0  = x `shiftL` (-d)
            | d >= 0 = x `shiftR` d 
    mask = (1 `shiftL` n) - 1

numbers = helper 0 (2^n-n) (2^(n-1))
  where
    helper n 0 0 = [n]
    helper _ _ 0 = []
    helper _ 0 _ = []
    helper n d d1 = helper (n*2) (d-1) d1 ++ helper (n*2+1) (d-1) (d1-1)

main = print $ sum $ filter check numbers
