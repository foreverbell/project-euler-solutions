
import Control.Monad (filterM, forM)
import Data.List (nub, find)

cubes = filter ((== 6) . length) $ filterM (const [True, False]) [0 .. 9]

solve = (flip div) 2 $ length . (filter id) $ do
    c1 <- cubes
    c2 <- cubes
    return $ checkCube c1 c2 where
        checkCube c1 c2 = and $ map (\x -> maybeToBool (find (== x) combs)) [ x^2 | x <- [1 .. 9] ] where
            combs = combCube c1 c2
            maybeToBool Nothing = False
            maybeToBool _ = True
        combCube c1 c2 = nub $ concat $ do
            c1' <- map warp c1
            c2' <- map warp c2
            x <- c1'
            y <- c2'
            return [x * 10 + y, y * 10 + x] 
            where
                warp 6 = [6, 9]
                warp 9 = [6, 9]
                warp x = [x]

main = print $ solve
