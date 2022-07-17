module Luhn (isValid) where

import qualified Data.Char as C

isValid :: String -> Bool
isValid n
  | length xs /= length n = False
  | otherwise = length zs > 1 && sum zs `mod` 10 == 0
  where
    xs = reverse [c | c <- n, C.isDigit c || C.isSpace c]
    ys = zip (filter C.isDigit xs) (cycle [1, 2])
    zs = [convert x y | (x, y) <- ys]

convert :: Char -> Int -> Int
convert c mul
  | mul == 2 && n > 4 = n * mul - 9
  | otherwise = n * mul
  where
    n = C.digitToInt c