module ProteinTranslation (proteins) where

import qualified Data.Map as M
import qualified Data.List as L

proteins :: String -> Maybe [String]
proteins xs = Just $ takeWhile (/= "STOP") ys
  where
    ch = chunks 3 xs
    ys = map (\x -> M.findWithDefault "STOP" x translations) ch

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
-- unfoldr builds a list from a seed value.
-- The function takes the element and returns Nothing if it is done producing the list 
-- or returns Just (a,b), in which case, a is a prepended to the list 
-- and b is used as the next element in a recursive call
chunks n xs = takeWhile (not . null) $ L.unfoldr (Just . splitAt n) xs

translations :: M.Map String String
translations =
  M.fromList
    [ ("AUG", "Methionine"),
      ("UUU", "Phenylalanine"),
      ("UUC", "Phenylalanine"),
      ("UUA", "Leucine"),
      ("UUG", "Leucine"),
      ("UCU", "Serine"),
      ("UCC", "Serine"),
      ("UCA", "Serine"),
      ("UCG", "Serine"),
      ("UAU", "Tyrosine"),
      ("UAC", "Tyrosine"),
      ("UGU", "Cysteine"),
      ("UGC", "Cysteine"),
      ("UGC", "Cysteine"),
      ("UGG", "Tryptophan")
    ]