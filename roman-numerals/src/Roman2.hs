module Roman2 (numerals) where

import qualified Data.Array as A
import qualified Data.List as L

{-
1. Find the floor of 'n' in the mapping, i.e. the highest
   decimal value 'v' that is less than or equal to 'n'.
2. Add the corresponding string to the answer, and subtract
   'v' from 'n'.
3. Repeat until n = 0.
-}
numerals :: Integer -> Maybe String
numerals n
  | n > 3000 = Nothing
  | otherwise = (Just . concat) $ L.unfoldr go n
  where
    go x =
      let k = idxOfFloor 0 y x xs
          (i, s) = (A.!) xs k
       in (if x > 0 then Just (s, x - i) else Nothing)
    y = length mapping - 1
    xs = A.listArray (0, y) mapping
    mapping =
      [ (1, "I"),
        (4, "IV"),
        (5, "V"),
        (9, "IX"),
        (10, "X"),
        (40, "XL"),
        (50, "L"),
        (90, "XC"),
        (100, "C"),
        (400, "CD"),
        (500, "D"),
        (900, "CM"),
        (1000, "M")
      ]

idxOfFloor :: Int -> Int -> Integer -> A.Array Int (Integer, String) -> Int
idxOfFloor lo hi k nums
  | hi - lo <= 1 = if null xs then -1 else head xs
  | otherwise = case () of
      _
        | e mid == k -> mid
        | e mid > k -> idxOfFloor lo (mid - 1) k nums
        | otherwise -> idxOfFloor mid hi k nums
  where
    e i = fst $ (A.!) nums i
    xs = [i | i <- [hi, hi - 1 .. lo], e i <= k]
    mid = lo + (hi - lo + 1) `div` 2
