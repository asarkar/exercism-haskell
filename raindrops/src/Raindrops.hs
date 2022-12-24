module Raindrops (convert) where

import qualified Data.List as L

convert :: Int -> String
convert n
  | null xs = show n
  | otherwise = xs
  where
    xs = concatMap i2s $ factors n

i2s :: Int -> String
i2s 3 = "Pling"
i2s 5 = "Plang"
i2s 7 = "Plong"
i2s _ = ""

-- search for the first divisor d in [2..sqrt(n)]
-- if d exists: return d : factors(n / d)
-- otherwise return n (since n is prime)
factors :: Int -> [Int]
factors = L.nub . go (2 : [3, 5 ..])
  where
    go [] n = [n]
    go (p : ps) n
      | p * p > n = [n]
      | n `mod` p == 0 = p : go (p : ps) (n `div` p)
      | otherwise = go ps n