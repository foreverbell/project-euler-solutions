module Common.MapReduce (
  mapReduce
, mapReduce'
) where

import Control.Parallel.Strategies (parMap, rdeepseq, using, NFData)
import Control.Parallel (pseq)

{-# INLINABLE divide #-}
{-# INLINABLE mapReduce #-}
{-# INLINABLE mapReduce' #-}

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = as : divide n bs where (as, bs) = splitAt n xs

mapReduce :: (NFData b, NFData c) => Int -> (a -> b) -> ([b] -> c) -> [a] -> c
mapReduce chunk mapFunc reduceFunc xs = mapResult `pseq` reduceResult
  where
    mapResult = concat $ parMap rdeepseq (map mapFunc) (divide chunk xs)
    reduceResult = reduceFunc mapResult `using` rdeepseq

mapReduce' :: (NFData b) => Int -> Int -> (a -> b) -> ([b] -> b) -> [a] -> b
mapReduce' block chunk mapFunc reduceFunc xs = fstReduce `pseq` sndReduce
  where
    fstReduce = map (mapReduce chunk mapFunc reduceFunc) (divide block xs)
    sndReduce = reduceFunc fstReduce `using` rdeepseq
