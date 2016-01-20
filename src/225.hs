import qualified Data.Set as S

test :: Int -> Bool
test modulo = go (1, 1, 1, S.empty)
  where
    go (a, b, c, coll) | S.member (a, b, c) coll = True
                       | d == 0 = False
                       | otherwise = go (b, c, d, S.insert (a, b, c) coll)
      where d = (a + b + c) `mod` modulo

main = print $ (filter test [3, 5 ..]) !! 123
