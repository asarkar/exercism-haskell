module Spiral (spiral) where

import qualified Data.List as L
import qualified Data.List.Split as LS

spiral :: Int -> [[Int]]
spiral n = LS.chunksOf n $ map snd $ L.sort xs
  where
    coord = spiral' n 0 0
    xs = zip coord [1 ..]

-- Generates the coordinates of the outer layer in spiral order,
-- then recurses into the inner matrix.
spiral' :: Int -> Int -> Int -> [(Int, Int)]
spiral' n row col
  | n <= 0 = []
  | n == 1 = [(row, col)]
  | otherwise = outer ++ spiral' (n - 2) (row + 1) (col + 1)
  where
    topRow = [(row, c) | c <- [col .. (col + n - 1)]]
    rightCol = [(r, col + n - 1) | r <- [(row + 1) .. (row + n - 1)]]
    bottomRow = [(row + n - 1, c) | c <- [(col + n - 2), (col + n - 3) .. col]]
    leftCol = [(r, col) | r <- [(row + n - 2), (row + n - 3) .. (row + 1)]]
    outer = topRow ++ rightCol ++ bottomRow ++ leftCol
