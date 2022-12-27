module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  -- uncurry converts a curried function to a function on pairs
  | m == n = Just $ length $ filter (uncurry (/=)) $ zip xs ys
  | otherwise = Nothing
  where
    m = length xs
    n = length ys
