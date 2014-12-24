import qualified Data.Text as T
import Data.List (group, sort, maximumBy)
import Data.Function (on)
import Data.Bits (xor)
import Data.Char (ord, chr, isAlpha)

group3 a b c _ [] = (reverse a, reverse b, reverse c)
group3 a b c n (x:xs) = case (n `mod` 3) of
    0 -> group3 (x:a) b c (n + 1) xs
    1 -> group3 a (x:b) c (n + 1) xs
    2 -> group3 a b (x:c) (n + 1) xs

comb3 [] [] [] = []
comb3 [a] [] [] = [a]
comb3 [a] [b] [] = [a, b]
comb3 (a:as) (b:bs) (c:cs) = a:b:c:(comb3 as bs cs)

findKey :: [Int] -> Int
findKey xs = maximumBy (compare `on` score) [(ord 'a') .. (ord 'z')]
    where
        score key = length $ filter (isAlpha . chr) $ map (xor key) xs 
        
solve :: String -> Int
solve input = sum result
    where
        t = T.pack input
        ts = T.split (== ',') t
        xs = map (read . (T.unpack)) ts :: [Int]
        (a,b,c) = group3 [] [] [] 0 xs
        decrypte xs = map (xor (findKey xs)) xs
        result = comb3 (decrypte a) (decrypte b) (decrypte c)
        
main = readFile "cipher1.txt" >>= (print . solve)
