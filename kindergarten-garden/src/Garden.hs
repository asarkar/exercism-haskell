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
garden students plants = Garden (M.fromList ys)
  where
    rows = map (chunks 2 . map letter2Plant) (lines plants)
    n = (length . head) rows
    pupils = take n $ L.sort students
    xs = map concat $ L.transpose rows
    ys = zip pupils xs

letter2Plant :: Char -> Plant
letter2Plant c = case c of
  'C' -> Clover
  'G' -> Grass
  'R' -> Radishes
  _ -> Violets

chunks :: Int -> [Plant] -> [[Plant]]
chunks _ [] = []
{-
unfoldr builds a list from a seed value.
The function takes the element and returns Nothing if it is done producing the list
or returns Just (a,b), in which case, a is a prepended to the list
and b is used as the next element in a recursive call
-}
chunks n xs = takeWhile (not . null) $ L.unfoldr (Just . splitAt n) xs

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) = M.findWithDefault [] student g