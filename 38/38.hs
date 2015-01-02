import Data.List (sort)

concatProduct :: Int -> String
concatProduct number = helper 1 number [] where
    helper dep number result
        | length result >= 9 = result
        | otherwise          = helper (dep + 1) number (result ++ (show $ dep * number))

main = print $ maximum magic where
    magic = [ read y :: Int | x <- [1 .. 9999], let y = concatProduct x, sort y == ['1'..'9'] ] 
