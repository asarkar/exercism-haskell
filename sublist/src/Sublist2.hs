module Sublist2 (sublist) where

-- Taken from the article https://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell
-- Explanation: https://stackoverflow.com/a/16700039/839733
data KMP a = KMP
  { done :: Bool,
    next :: a -> KMP a
  }

sublist :: (Ord a) => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | isSublist xs ys = Just $ if xs == ys then EQ else LT
  | isSublist ys xs = Just GT
  | otherwise = Nothing

isSublist :: (Eq a) => [a] -> [a] -> Bool
isSublist = (any done .) . scanl next . makeTable

makeTable :: (Eq a) => [a] -> KMP a
makeTable xs = table
  where
    table = makeTable' xs (const table)

makeTable' :: (Eq a) => [a] -> (a -> KMP a) -> KMP a
makeTable' [] failure = KMP True failure
makeTable' (x : xs) failure = KMP False test
  where
    test c = if c == x then success else failure c
    success = makeTable' xs (next (failure x))
