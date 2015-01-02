-- http://english.stackexchange.com/questions/111765/how-to-write-out-numbers-in-compliance-with-british-usage

numberToWord :: Int -> String

numberToWord x
    | x == 0 = "zero"
    | x == 1 = "one"
    | x == 2 = "two"
    | x == 3 = "three"
    | x == 4 = "four"
    | x == 5 = "five"
    | x == 6 = "six"
    | x == 7 = "seven"
    | x == 8 = "eight"
    | x == 9 = "nine"
    | x == 10 = "ten"
    | x == 11 = "eleven"
    | x == 12 = "twelve"
    | x == 13 = "thirteen"
    | x == 14 = "fourteen"
    | x == 15 = "fifteen"
    | x == 16 = "sixteen"
    | x == 17 = "seventeen"
    | x == 18 = "eighteen"
    | x == 19 = "nineteen"
    | x == 20 = "twenty"
    | x == 30 = "thirty"
    | x == 40 = "forty"
    | x == 50 = "fifty"
    | x == 60 = "sixty"
    | x == 70 = "seventy"
    | x == 80 = "eighty"
    | x == 90 = "ninety"
    | x == 1000 = "one thousand"
    | x `mod` 100 == 0 = (numberToWord (x `div` 100)) ++ " hundred"
    | x < 100 = (numberToWord (x - (x `mod` 10))) ++ " " ++ (numberToWord (x `mod` 10))
    | otherwise = (numberToWord (x - (x `mod` 100))) ++ " and " ++ (numberToWord (x `mod` 100)) 

countLetters x = foldr ((+) . length) 0 (words (numberToWord x))

main = print s where
    s = sum list
    list = map countLetters [1 .. 1000]

