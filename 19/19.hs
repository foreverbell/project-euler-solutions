-- 1 Jan 1901 was a Tuesday
-- ModifiedJulianDay 15385  ->  1901-01-01
-- ModifiedJulianDay 51909  ->  2000-12-31

import Data.Time.Calendar

firstDay :: Day -> Bool
firstDay date = (d == 1)
    where (y, m, d) = toGregorian date

main = do
    print (length (filter (\x -> (firstDay (ModifiedJulianDay x)) && (x - 15385) `mod` 7 == 5) [15385 .. 51909]))
