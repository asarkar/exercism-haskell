module Series (Error (..), largestProduct) where

import qualified Data.Char as C
import qualified Data.List as L

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

type Product = Either Error Integer

largestProduct :: Int -> String -> Product
largestProduct n digits
  | n < 0 || n > length digits = Left InvalidSpan
  | otherwise = case invalid of
      Just x -> Left (InvalidDigit x)
      _ -> case map pdt xxs of
        [] -> Right 1
        xs -> Right (L.maximum xs)
  where
    sliding [] = []
    sliding xs = take n xs : sliding (tail xs)
    toI = toInteger . C.digitToInt
    pdt = foldl ((. toI) . (*)) 1
    xxs = filter ((>= n) . length) (sliding digits)
    invalid = L.find (not . C.isDigit) digits
