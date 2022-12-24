module Scrabble (scoreLetter, scoreWord) where

import qualified Data.Char as C

scoreLetter :: Char -> Integer
scoreLetter letter
  | ch `elem` ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'] = 1
  | ch `elem` ['D', 'G'] = 2
  | ch `elem` ['B', 'C', 'M', 'P'] = 3
  | ch `elem` ['F', 'H', 'V', 'W', 'Y'] = 4
  | ch == 'K' = 5
  | ch `elem` ['J', 'X'] = 8
  | ch `elem` ['Q', 'Z'] = 10
  | otherwise = 0
  where
    ch = C.toUpper letter

scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word