module Garden
  ( Plant (..),
    garden,
    lookupPlants,
  )
where

import qualified Data.List as L
import qualified Data.Map as M

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

newtype Garden = Garden (M.Map String [Plant]) deriving (Show)

garden :: [String] -> String -> Garden
garden students plants = Garden g
  where
    rows = map (chunks 2 . map letter2Plant) (lines plants)
    n = (length . head) rows
    pupils = take n $ L.sort students
    (r1, r2) = (head rows, last rows)
    g = M.fromList (students2Plants pupils r1 r2)

letter2Plant :: Char -> Plant
letter2Plant c = case c of
  'C' -> Clover
  'G' -> Grass
  'R' -> Radishes
  _ -> Violets

chunks :: Int -> [Plant] -> [[Plant]]
chunks _ [] = []
-- unfoldr builds a list from a seed value.
-- The function takes the element and returns Nothing if it is done producing the list
-- or returns Just (a,b), in which case, a is a prepended to the list
-- and b is used as the next element in a recursive call
chunks n xs = takeWhile (not . null) $ L.unfoldr (Just . splitAt n) xs

students2Plants :: [String] -> [[Plant]] -> [[Plant]] -> [(String, [Plant])]
students2Plants [] _ _ = []
students2Plants (s : xs) (p1 : ys) (p2 : zs) = (s, p1 ++ p2) : students2Plants xs ys zs
students2Plants _ _ _ = []

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) = M.findWithDefault [] student g