module Phone (number) where

import qualified Data.Char as C

number :: String -> Maybe String
number xs
  | n == 11 && head ys == '1' = number $ tail ys
  | n == 10 && isValid ys = Just ys
  | otherwise = Nothing
  where
    ys = [c | c <- xs, C.isDigit c]
    n = length ys

isValid :: String -> Bool
isValid xs
  | n == 10 = isValid areaCode && isValid exchangeCode && isValid subscriberNum
  | n == 3 = head xs >= '2'
  | n == 4 = True
  | otherwise = False
  where
    n = length xs
    (areaCode, localNum) = splitAt 3 xs
    exchangeCode = take 3 localNum
    subscriberNum = drop 3 localNum