module RunLength (decode, encode) where

import qualified Data.Char as C
import qualified Data.List as L

decode :: String -> String
decode xs
  | n <= 1 = xs
  | null digits = head xs : decode (tail xs)
  | otherwise = replicate i (head rest) ++ decode (tail rest)
  where
    n = length xs
    digits = takeWhile C.isDigit xs
    i = read digits :: Int
    rest = dropWhile C.isDigit xs

f :: String -> String
f xs
  | n <= 1 = xs
  | otherwise = show n ++ [head xs]
  where
    n = length xs

encode :: String -> String
encode xs = concatMap f (L.group xs)
