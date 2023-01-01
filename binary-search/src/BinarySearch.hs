module BinarySearch (find) where

import qualified Data.Array as A

find :: Ord a => A.Array Int a -> a -> Maybe Int
find xs = find' 0 (length xs - 1) xs

find' :: Ord a => Int -> Int -> A.Array Int a -> a -> Maybe Int
find' lo hi xs x
  | hi < lo = Nothing
  | e mid == x = Just mid
  | e mid > x = find' lo (mid - 1) xs x
  | otherwise = find' (mid + 1) hi xs x
  where
    e = (A.!) xs
    mid = lo + (hi - lo + 1) `div` 2
