-- http://english.stackexchange.com/questions/111765/how-to-write-out-numbers-in-compliance-with-british-usage

numberToWord :: Int -> String
numberToWord 0 = "zero"
numberToWord 1 = "one"
numberToWord 2 = "two"
numberToWord 3 = "three"
numberToWord 4 = "four"
numberToWord 5 = "five"
numberToWord 6 = "six"
numberToWord 7 = "seven"
numberToWord 8 = "eight"
numberToWord 9 = "nine"
numberToWord 10 = "ten"
numberToWord 11 = "eleven"
numberToWord 12 = "twelve"
numberToWord 13 = "thirteen"
numberToWord 14 = "fourteen"
numberToWord 15 = "fifteen"
numberToWord 16 = "sixteen"
numberToWord 17 = "seventeen"
numberToWord 18 = "eighteen"
numberToWord 19 = "nineteen"
numberToWord 20 = "twenty"
numberToWord 30 = "thirty"
numberToWord 40 = "forty"
numberToWord 50 = "fifty"
numberToWord 60 = "sixty"
numberToWord 70 = "seventy"
numberToWord 80 = "eighty"
numberToWord 90 = "ninety"
numberToWord 1000 = "one thousand"
numberToWord x
    | x `mod` 100 == 0 = (numberToWord (x `div` 100)) ++ " hundred"
    | x < 100 = (numberToWord (x - (x `mod` 10))) ++ " " ++ (numberToWord (x `mod` 10))
    | otherwise = (numberToWord (x - (x `mod` 100))) ++ " and " ++ (numberToWord (x `mod` 100)) 

countLetters x = foldr ((+) . length) 0 (words (numberToWord x))

main = print $ sum $ map countLetters [1 .. 1000]
