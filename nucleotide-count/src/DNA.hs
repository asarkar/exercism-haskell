module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Right (M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
nucleotideCounts [x]
  | x == 'A' = Right (M.singleton A 1)
  | x == 'C' = Right (M.singleton C 1)
  | x == 'G' = Right (M.singleton G 1)
  | x == 'T' = Right (M.singleton T 1)
  | otherwise = Left "error"
nucleotideCounts (x : xs) = M.unionWith (+) <$> nucleotideCounts [x] <*> nucleotideCounts xs
