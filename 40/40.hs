import Data.Char

main = do
    print $ (product . map digitToInt) [ numberSeq!!1, numberSeq!!10, numberSeq!!100, numberSeq!!1000, numberSeq!!10000, numberSeq!!100000, numberSeq!!1000000 ]
    where
        numberSeq = '0' : (take 1000000 $ concatMap show [1 .. ])
