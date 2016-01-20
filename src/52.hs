import Data.List (group, sort)

check number = (length . group) ps == 1
    where ps = map (sort . (\x -> show (number * x))) [1 .. 6]

main = print $ head $ dropWhile (not . check) [100002, 100005 .. ]

