module CryptoSquare (encode) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S

encode :: String -> String
encode xs = case ys of
  [] -> []
  -- Join lists with separator: https://stackoverflow.com/a/9221026/839733
  -- https://hackage.haskell.org/package/split
  _ -> unwords cipher
  where
    ys = normalize xs
    -- Lazy; doesn't evaluate the following if 'ys' is empty
    -- https://stackoverflow.com/a/72887963/839733
    col = (size . length) ys
    rect = S.chunksOf col ys
    cipher = L.transpose $ map (padR col) rect

normalize :: String -> String
normalize = map C.toLower . filter C.isAlphaNum

size :: Int -> Int
size n = col
  where
    x = fromIntegral n :: Float
    col = ceiling $ sqrt x

padR :: Int -> String -> String
padR n xs = xs ++ replicate (n - x) ' '
  where
    x = length xs
