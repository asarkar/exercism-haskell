module Matrix (saddlePoints) where

-- https://mmhaskell.com/data-structures/array
import qualified Data.Array as A
import qualified Data.Ix as Ix

-- Good functional solution
-- https://exercism.org/tracks/haskell/exercises/saddle-points/solutions/cornishon
saddlePoints :: (Ix.Ix i, Ord e) => A.Array (i, i) e -> [(i, i)]
saddlePoints matrix = [a | (a, x) <- xs, isSaddle a x]
  where
    xs = A.assocs matrix
    ((rowStart, colStart), (rowEnd, colEnd)) = A.bounds matrix
    rowBnd = (rowStart, rowEnd)
    rowRng = Ix.range rowBnd
    colBnd = (colStart, colEnd)
    colRng = Ix.range colBnd
    f x y = matrix A.! (x, y)
    findMax x = maximum $ map (f x) colRng
    findMin x = minimum $ map (`f` x) rowRng
    rowMax = A.listArray rowBnd $ map findMax rowRng
    colMin = A.listArray colBnd $ map findMin colRng
    isSaddle (r, c) x = (x >= rowMax A.! r) && (x <= colMin A.! c)
