
import qualified Data.Set as S
import Data.Array
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad

primesTo m = map fst $ filter (id . snd) $ assocs $ runSTArray $ do
    sieve <- newArray (2, m) True
    let root = (floor . sqrt . fromIntegral) m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve
        
primesN = take 500500 $ primesTo (500500 * 15)

modulo = 500500507

mulMod a b m = (a * b) `mod` m

powMod a 0 m = 1
powMod a 1 m = a `mod` m
powMod a p m = case even p of
    True  -> mulMod half half m
    False -> mulMod (mulMod half half m) a m 
    where half = powMod a (p `div` 2) m

data Item = Item {
    base :: Int,
    expLog2 :: Int
} deriving (Eq)

ordKey :: Item -> Double
ordKey x = fromIntegral (expLog2 x) + logBase 2 (log ((fromIntegral . base) x))

instance Ord Item where
    a `compare` b = (ordKey a) `compare` (ordKey b)

solveIter :: Int -> S.Set Item -> Int
solveIter 0 items = foldl helper 1 (S.toList items) where
    helper accum item = mulMod accum (powMod (base item) (2 ^ (expLog2 item) - 1) modulo) modulo
solveIter count items = solveIter (count - 1) items'' where
    minItem = S.findMin items
    items' = S.deleteMin items
    items'' = S.insert (Item (base minItem) ((expLog2 minItem) + 1)) items'

solve = solveIter 500500 $ S.fromList (zipWith Item primesN (repeat 0))

main = print solve

