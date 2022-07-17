module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = divBy year 400 || (divBy year 4 && not (divBy year 100))
  where
    divBy n m = n `rem` m == 0
