
import Common.Numbers (powMod)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

dynamic :: Int -> Int -> V.Vector Int -> Int
dynamic _ 0 dp = V.head dp
dynamic modulo x dp = dynamic modulo (x - 1) dp' where
    dp' = V.fromList $ map (\i -> (dp!i + dp!((i - r) `mod` n)) `mod` modulo) [0 .. n - 1]  
    r = powMod x x 250
    n = V.length dp

solve n modulo = pred $ dynamic modulo n dp where 
    dp = V.fromList (1 : replicate 249 0)

main = print $ solve 250250 (10^16)
