module LeapYear (isLeapYear) where

divBy :: Integer -> Integer -> Bool
divBy n m = n `rem` m == 0

isLeapYear :: Integer -> Bool
isLeapYear year = divBy year 400 || (divBy year 4 && not (divBy year 100))
