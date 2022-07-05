module CollatzConjecture (collatz) where

-- There are some kick-a$$ community solutions.
-- https://exercism.org/tracks/haskell/exercises/collatz-conjecture/solutions/modul
-- succ <$> stands for fmap (+1) $, which is incrementing the value inside Just.
-- Since the recursion ends in 1, we get the count as desired.
collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (collatz' n 0)

collatz' :: Integer -> Integer -> Integer
collatz' n count
  | n == 1 = count
  | even n = collatz' (n `div` 2) (count + 1)
  | otherwise = collatz' (3 * n + 1) (count + 1)
