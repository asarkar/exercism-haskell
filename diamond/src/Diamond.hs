module Diamond (diamond) where

import qualified Data.Char as C
import qualified Data.List as L

diamond :: Char -> Maybe [String]
diamond c
  | C.isAlpha c = Just $ top ++ bottom
  | otherwise = Nothing
  where
    height = C.ord c - C.ord 'A' + 1
    top = map (row height) [0 .. height - 1]
    bottom = (tail . reverse) top

row :: Int -> Int -> String
row n i = half ++ (tail . reverse) half
  where
    ch = C.chr (C.ord 'A' + i)
    k = n - i - 1
    go x = if x == k then ch else ' '
    half = take n $ L.unfoldr (\y -> Just (go y, y + 1)) 0