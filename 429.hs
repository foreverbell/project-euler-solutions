import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad

primesTo m = map fst $ filter (id . snd) $ assocs $ runSTUArray $ do
    sieve <- newArray (2, m) True
    let root = (floor . sqrt . fromIntegral) m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve

powMod a 0 m = 1
powMod a 1 m = a `mod` m
powMod a p m = case even p of
    True  -> mulMod half half m
    False -> mulMod (mulMod half half m) a m 
    where half = powMod a (p `div` 2) m
          mulMod a b m = (a * b) `mod` m

solve n modulo = productMod modulo $ map (\x -> ((powMod x (countExponent n x) modulo)^2 + 1) `mod` modulo) primes where
    productMod m xs = foldl helper 1 xs where
        helper accum x = (accum * x) `mod` m
    primes = primesTo n
    countExponent n p = sum $ takeWhile (/= 0) rs where
        rs = (n `div` p) : map (\x -> x `div` p) rs

main = print $ solve (10^8) (10^9+9)
