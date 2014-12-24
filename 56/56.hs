
sumOfDigit :: Integer -> Integer
sumOfDigit = sum . map (read . (\x -> [x])) . show 

main = do
    print $ maximum [ sumOfDigit (a^b) | a <- [1 .. 99], b <- [1 .. 99] ]
