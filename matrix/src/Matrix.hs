module Matrix
  ( Matrix,
    cols,
    column,
    flatten,
    fromList,
    fromString,
    reshape,
    row,
    rows,
    shape,
    transpose,
  )
where

import qualified Control.Arrow as A
import qualified Data.Vector as V

newtype Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m)
  | null m = 0
  | otherwise = length $ V.head m

column :: Int -> Matrix a -> V.Vector a
column x (Matrix m) = V.map (\r -> (V.!) r (x - 1)) m

flatten :: Matrix a -> V.Vector a
flatten (Matrix m) = V.foldl (V.++) V.empty m

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, c) = Matrix . chunks c . flatten
  where
    chunks n xs
      | null xs = V.empty
      | otherwise = V.takeWhile (not . null) $ V.unfoldr (Just . V.splitAt n) xs

row :: Int -> Matrix a -> V.Vector a
row x (Matrix m) = (V.!) m (x - 1)

rows :: Matrix a -> Int
rows (Matrix m) = length m

shape :: Matrix a -> (Int, Int)
shape = rows A.&&& cols

transpose :: Matrix a -> Matrix a
transpose m = Matrix $ V.fromList $ map (`column` m) [1 .. cols m]

{-
https://exercism.org/tracks/haskell/exercises/matrix/solutions/twrch-trwyth
(head &&& tail) [1,2,3] = (1,[2,3])
unzip takes a list of pairs and creates a pair of lists, so
unzip [(1,[2,3]) (4,[5,6])] = ([1,4],[[2,3],[5,6]])

transpose :: Matrix a -> Matrix a
transpose matrix
  | cols matrix == 0 = V.empty
  | otherwise = V.cons firstCol $ transpose rest
  where
    (firstCol, rest) = V.unzip $ V.map (V.head A.&&& V.tail) matrix
-}