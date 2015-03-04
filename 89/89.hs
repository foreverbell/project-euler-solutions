
roman :: String -> String
roman ('I':'I':'I':'I':r)      = 'I':'V':roman r
roman ('V':'I':'I':'I':'I': r) = 'I':'X':roman r
roman ('X':'X':'X':'X':r)      = 'X':'L':roman r
roman ('L':'X':'X':'X':'X':r)  = 'X':'C':roman r
roman ('C':'C':'C':'C':r)      = 'C':'D':roman r
roman ('D':'C':'C':'C':'C':r)  = 'C':'M':roman r
roman (x:r) = x:roman r
roman [] = []

solve :: String -> Int
solve s = count r - count r' where
    r = words s
    r' = map roman r
    count = sum . (map length)

main = (readFile "p089_roman.txt") >>= (print . solve)
