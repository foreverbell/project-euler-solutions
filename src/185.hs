import qualified Data.IntMap as M
import           Control.Monad (guard)
import           Control.Applicative (empty)

{- real 3m37.442s
   user 3m36.196s
   sys  0m1.135s
-} 

type Input = [(Int, Int)]

{-# INLINE encode #-}
{-# INLINE decodeBase #-}
{-# INLINE coincide #-}

encode :: [Int] -> [Int] -> Int
encode xs cs = foldr f 0 $ zip xs cs
  where
    f (a, b) v | b > a = -1
               | v < 0 = v
               | otherwise = v * (a + 1) + b

decodeBase :: Int -> [Int]
decodeBase x = take 8 $ helper x
  where helper x = let (q, r) = x `quotRem` 10 in r : helper q

coincide :: [Int] -> [Int] -> Int
coincide as bs = length $ filter id $ zipWith (==) as bs

search1 :: Input -> M.IntMap Int
search1 input = M.fromList $ do
  a <- [0 .. 99999999]
  let e = encode cs $ map (coincide (decodeBase a)) vsd
  guard $ e >= 0
  return (e, a)
  where vs = map ((`rem` 10^8) . fst) input
        cs = map snd input
        vsd = map decodeBase vs

search2 :: Input -> M.IntMap Int -> (Int, Int)
search2 input m = head $ do
  a <- [0 .. 99999999]
  let e = encode cs $ map (coincide (decodeBase a)) vsd
  guard $ e >= 0
  case M.lookup (mv - e) m of
       Just f -> return (f, a)
       Nothing -> empty
  where vs = map ((`quot` 10^8) . fst) input
        cs = map snd input
        vsd = map decodeBase vs
        mv = encode cs cs

solve :: Input -> Int
solve input = b * (10^8) + a
  where (a, b) = search2 input (search1 input)

parse :: String -> Input
parse input = map f (map read <$> map words (lines input))
  where f xs = (xs !! 0, xs !! 1)

main = (print . solve . parse) =<< readFile "input/p185_input.txt"
