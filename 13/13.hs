import System.IO

main = do
    input <- readFile "input.txt"
    let numbers = map (\x -> read x :: Integer) (lines input)
    let s = sum numbers 
    print (take 10 (show s)) 

