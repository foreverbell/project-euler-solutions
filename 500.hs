
import Common.Primes (primesTo)
import Common.Numbers (powMod)
import qualified Data.Set as S
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad

primesN = take 500500 $ primesTo (500500 * 15)

modulo = 500500507

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
    helper accum item = accum * (powMod (base item) (2 ^ (expLog2 item) - 1) modulo) `mod` modulo
solveIter count items = solveIter (count - 1) items'' where
    minItem = S.findMin items
    items' = S.deleteMin items
    items'' = S.insert (Item (base minItem) ((expLog2 minItem) + 1)) items'

solve = solveIter 500500 $ S.fromList (zipWith Item primesN (repeat 0))

main = print solve

