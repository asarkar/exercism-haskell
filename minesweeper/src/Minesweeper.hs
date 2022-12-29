module Minesweeper (annotate) where

import qualified Data.Maybe as M
import qualified Data.Vector as V

type Cell = (Int, Int)

type Board = V.Vector (V.Vector Char)

annotate :: [String] -> [String]
annotate board = map (concat . go) [0, 1 .. m]
  where
    go row = map (\col -> countMines board' (row, col)) [0, 1 .. n]
    board' = V.fromList $ map V.fromList board
    m = length board - 1
    n = if null board then -1 else length (head board) - 1

charAt :: Board -> Cell -> Maybe Char
charAt board (r, c) = (V.!?) board r >>= flip (V.!?) c

neighbors :: Cell -> [Cell]
neighbors (r, c) = [(x, y) | x <- go r, y <- go c, (x, y) /= (r, c)]
  where
    go x = map (x +) [-1, 0, 1]

countMines :: Board -> Cell -> String
countMines board cell = case M.fromJust ch of
  '*' -> "*"
  _ -> y
  where
    ch = charAt board cell
    x = (sum . map go) (neighbors cell)
    y = case x of
      0 -> " "
      _ -> show x
    go c = case charAt board c of
      Just a -> fromEnum (a == '*')
      _ -> 0
