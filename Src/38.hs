import Data.List (sort)
import Control.Monad (guard)

concatProduct :: Int -> String
concatProduct n = head $ dropWhile (\s -> (length s) < 9) prefix where
    prefix :: [String]
    prefix = [] : zipWith (++) prefix [ show (n * i) | i <- [1 .. 9] ]

main = print $ maximum $ do
    x <- [1 .. 9999]
    let y = concatProduct x
    guard $ sort y == ['1' .. '9']
    return ((read y) :: Int)
