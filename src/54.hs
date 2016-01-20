import Data.List 
import Data.Function (on)
import Data.Char (ord)
import Data.Maybe (fromJust)

type Card = (Int, Char)

cardValue :: Card -> Int
cardValue = fst

cardSuit :: Card -> Char
cardSuit = snd

sameSuit :: [Card] -> Bool
sameSuit cs = length (groupBy (\c1 c2 -> cardSuit c1 == cardSuit c2) cs) == 1

consecutiveValue :: [Card] -> Bool
consecutiveValue cs = vs == [head vs .. last vs] where 
    vs = sort $ map cardValue cs

highCard :: [Card] -> Maybe [Int]
highCard cs = Just $ reverse vs where 
    vs = sort $ map cardValue cs

onePair :: [Card] -> Maybe [Int]
onePair cs = case eq2 of
    Nothing -> Nothing
    Just x -> Just $ (head x) : (reverse (filter (/= (head x)) vs)) 
    where 
        vs = sort $ map cardValue cs
        g  = group vs
        eq2 = find (\x -> length x == 2) g

twoPairs :: [Card] -> Maybe [Int]
twoPairs cs
    | length g == 3 = Just [max p1 p2, min p1 p2, rest]
    | otherwise = Nothing where 
        vs = sort $ map cardValue cs
        g  = group vs
        g1 = filter (\x -> length x == 1) g
        g2 = filter (\x -> length x == 2) g
        p1 = head (head g2)
        p2 = head (last g2)
        rest = head (head g1)

threeOfKind :: [Card] -> Maybe [Int]
threeOfKind cs
    | not (null g3) = Just $ x : (reverse (filter (/= x) vs))
    | otherwise = Nothing where 
        vs = sort $ map cardValue cs
        g3 = filter (\x -> length x == 3) (group vs)
        x = head (head g3)

straight :: [Card] -> Maybe [Int]
straight cs
    | consecutiveValue cs = Just $ reverse $ sort $ map cardValue cs
    | otherwise = Nothing

flush :: [Card] -> Maybe [Int]
flush cs
    | sameSuit cs = Just $ reverse $ sort $ map cardValue cs
    | otherwise = Nothing

fullHouse :: [Card] -> Maybe [Int]
fullHouse cs
    | length g == 2 && length g2 == 2 = Just [head g3, head g2]
    | otherwise = Nothing where 
        vs = sort $ map cardValue cs
        g  = sortBy (compare `on` length) (group vs)
        g2 = head g
        g3 = last g

fourOfKind :: [Card] -> Maybe [Int]
fourOfKind cs
    | length g == 2 && length g1 == 1 = Just [head g4, head g1]
    | otherwise = Nothing where
        vs = sort $ map cardValue cs
        g  = sortBy (compare `on` length) (group vs)
        g1 = head g
        g4 = last g

straightFlush :: [Card] -> Maybe [Int]
straightFlush cs
    | consecutiveValue cs && sameSuit cs = Just $ reverse $ sort $ map cardValue cs
    | otherwise = Nothing

royalFlush :: [Card] -> Maybe [Int]
royalFlush cs 
    | consecutiveValue cs && sameSuit cs && vs!!0 == 10 = Just []
    | otherwise = Nothing where 
        vs = sort $ map cardValue cs

parseCard :: String -> Card
parseCard [a,b] = (v, b) where 
    v
        | a == 'T' = 10
        | a == 'J' = 11
        | a == 'Q' = 12
        | a == 'K' = 13
        | a == 'A' = 14
        | otherwise = ord a - ord '0'

calScore :: [Card] -> (Int, [Int])
calScore c = (10 - index, fromJust (result!!index)) where
    func = [royalFlush, straightFlush, fourOfKind, fullHouse, flush, straight, threeOfKind, twoPairs, onePair, highCard]
    result = [ f c | f <- func ]
    helper mxs = case mxs of
                    Nothing -> False
                    otherwise -> True
    index = fromJust (findIndex helper result)

is1Winner :: [Card] -> [Card] -> Bool
is1Winner c1 c2 = (calScore c1) > (calScore c2)

solveSingle :: String -> Bool
solveSingle s = is1Winner c1 c2 where
    cards = map parseCard $ filter (\s -> length s == 2) $ groupBy (\c1 c2 -> (c1 == ' ') == (c2 == ' ')) s
    c1 = take 5 cards
    c2 = drop 5 cards

solve :: String -> Int
solve input = foldl (\s x -> s + (if x then 1 else 0)) 0 win
    where win = map solveSingle (lines input)

main = readFile "input/p054_poker.txt" >>= (print . solve)
