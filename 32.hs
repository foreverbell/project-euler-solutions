import Data.List

findAll :: Int -> [Int]
findAll a = [ a*b | b <- [1 .. 987], check a b ] where
    check a b = ['1' .. '9'] == (sort $ (show a) ++ (show b) ++ (show $ a*b))

main = print $ sum l where
    l = nub $ foldl' (++) [] [ findAll x | x <- [1 .. 9876]  ]
