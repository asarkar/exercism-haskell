module Sublist (sublist) where

import Control.Arrow ((&&&))
import qualified Data.List as L

sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | smaller `elem` xxs = Just $ compare x y
  | otherwise = Nothing
  where
    (x, y) = (length xs, length ys)
    smaller = if x <= y then xs else ys
    larger = if x > y then xs else ys
    n = length smaller
    xxs = takeWhile ((== n) . length) $ L.unfoldr (Just . (take n &&& tail)) larger
