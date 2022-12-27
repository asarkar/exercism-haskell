module Triangle (rows) where

import qualified Data.List as L

rows :: Int -> [[Integer]]
rows x = map row [0 .. x - 1]

{-
Identity: C(n,k+1) = C(n,k) * (n-k) / (k+1), where n starts from 0.
We start with C(n,0) = 1, and calculate the rest using the identity.
But wait...each row is mirrored around the middle element, so,
we only need to calculate up to the middle element. Then we
flip the row and append to itself.
-}
row :: Int -> [Integer]
row n = left ++ right
  where
    mid = n `div` 2
    left = 1 : take mid (L.unfoldr (go n) (0, 1))
    y = mid + (n `mod` 2)
    right = reverse $ take y left

go :: Int -> (Int, Integer) -> Maybe (Integer, (Int, Integer))
go n (i, x) = Just (y, (i + 1, y))
  where
    a = toInteger (n - i)
    b = toInteger (i + 1)
    y = x * a `div` b
