module Raindrops (convert) where

convert :: Int -> String
convert n
  | null ys = show n
  | otherwise = concat ys
  where
    xs = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    ys = [y | (x, y) <- xs, n `mod` x == 0]
