module Series (slices) where

import qualified Data.Char as C
import Data.Sequence (Seq, ViewL ((:<)), ViewR ((:>)), (|>))
import qualified Data.Sequence as S

type Slice = Seq Int

type Slices = Seq Slice

{-
We start with a slice of size n. Then for each of the
remaining characters, we drop the head from n and append
the character.
We use a Seq instead of a list because appending to a
list is an O(n) time operation.
-}
slices :: Int -> String -> Slices
slices n xs
  | n > x = S.empty
  | n == 0 = S.replicate (x + 1) S.empty
  | otherwise = foldl go (S.singleton left) right
  where
    x = length xs
    ys = S.fromList $ map C.digitToInt xs
    (left, right) = S.splitAt n ys

go :: Slices -> Int -> Slices
go xs i = xs |> (zs |> i)
  where
    slice = case S.viewr xs of
      -- '(:>)' is a data constructor of 'ViewR'
      _ :> ys -> ys
      _ -> error "Empty seq"
    zs = case S.viewl slice of
      -- '(:<)' is a data constructor of 'ViewL'
      _ :< ys -> ys
      _ -> error "Empty slice"