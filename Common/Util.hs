module Common.Util (
    isqrt
) where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
