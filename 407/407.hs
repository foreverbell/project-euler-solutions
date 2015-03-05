
import Data.List (maximum)
import Data.Array
import Data.Array.ST
import Control.Monad (forM_, liftM, filterM, when)
import Control.Monad.ST

data SieveItem = SieveItem {
    isP :: Bool,
    minP :: Int,
    minPExp :: Int
} deriving (Show)

maxN = 10^7
sieve = eratos maxN

eratos :: Int -> Array Int SieveItem
eratos n = runSTArray $ do
    sieve <- newListArray (2, n) $ zipWith3 SieveItem (repeat True) [2 .. n] (repeat 1)
    forM_ [2 .. n] $ \i -> do
        isPrime <- liftM isP (readArray sieve i)
        when isPrime $ do
            forM_ [i^2, i^2 + i .. n] $ \j -> do
                isPrime' <- liftM isP (readArray sieve j)
                when isPrime' $ do
                    let q = j `div` i
                    qp <- liftM minP (readArray sieve q)
                    qExp <- liftM minPExp (readArray sieve q)
                    let jExp = if qp == i then (qExp + 1) else 1
                    writeArray sieve j $ SieveItem False i jExp
    return sieve 

exgcd :: Int -> Int -> (Int, Int, Int)
exgcd a 0 = (a, 1, 0)
exgcd a b = (d, y, x - (a `div` b) * y) where
    (d, x, y) = exgcd b (a `mod` b)

modularInverse :: Int -> Int -> Int
modularInverse x m = if d /= 1
    then undefined
    else a `mod` m
    where (d, a, b) = exgcd x m

maxIdempotent 1 = 0
maxIdempotent n = maximum idempotents
    where
        factorize 1 = []
        factorize n = p : (factorize (n `div` p)) where p = (minP (sieve!n)) ^ (minPExp (sieve!n))
        primitiveFactors = factorize n
        factors = map product (properPowerset primitiveFactors)
        idempotents = 1 : [ inv * p | p <- factors, let q = n `div` p, let inv = modularInverse p q ]
        powerset xs = filterM (const [True, False]) xs
        properPowerset [] = []
        properPowerset (x:[]) = []
        properPowerset xs = tail $ reverse $ tail $ powerset xs

main = print $ sum $ map maxIdempotent [1 .. maxN]
