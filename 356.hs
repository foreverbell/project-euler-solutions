
import qualified Data.Matrix as M
import Data.Maybe (fromJust)

modulo = 10^8 :: Int

newtype QuotientRing = QuotientRing Int

quotient (QuotientRing x) = x `mod` modulo

instance Show QuotientRing where
    show a = show $ quotient a

instance Num QuotientRing where
    fromInteger x = QuotientRing (fromInteger x)
    a + b = QuotientRing $ ((quotient a) + (quotient b)) `mod` modulo where
    a * b = QuotientRing $ ((quotient a) * (quotient b)) `mod` modulo where
    negate a = let a' = quotient a in QuotientRing $ (modulo - a') `mod` modulo
    abs a = undefined
    signum a = undefined

powMat :: (Num a) => M.Matrix a -> Int -> Maybe (M.Matrix a)
powMat a p = if (n == m) 
    then Just $ helper a p (M.identity n) 
    else Nothing
    where
        n = M.ncols a
        m = M.nrows a
        helper a 0 r = r
        helper a p r = helper (M.multStd a a) (p `div` 2) r' where
            r' = if (odd p) then M.multStd r a else r

solve n = ((M.multStd (fromJust (powMat mat 987654321)) initial) M.! (1, 1)) - 1 where
    mat = M.fromList 3 3 $ map QuotientRing [0, 1, 0, 0, 0, 1, -n, 0, 2^n]
    initial = M.fromList 3 1 $ map QuotientRing [3, 2^n, 4^n]

main = print $ sum [ solve i | i <- [1 .. 30] ]
