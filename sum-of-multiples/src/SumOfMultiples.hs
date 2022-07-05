module SumOfMultiples (sumOfMultiples) where

import qualified Data.List as L

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum z
  where
    x = concat [fact i limit | i <- factors]
    y = L.group (L.sort x)
    z = [head xs | xs <- y]

fact :: Integer -> Integer -> [Integer]
fact n limit
  | n == 0 = [0]
  | otherwise = takeWhile (< limit) (map (* n) [1 ..])