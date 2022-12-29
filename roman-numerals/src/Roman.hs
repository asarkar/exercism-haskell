module Roman (numerals) where

import qualified Data.List as L

{-
1. Find the floor of 'n' in the mapping, i.e. the highest
   decimal value 'v' that is less than or equal to 'n'.
2. Add the corresponding string to the answer, and subtract
   'v' from 'n'.
3. Repeat until n = 0.

<$> from Functor, fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b

<> from Monoid, mappend
(<>) :: Monoid m => m -> m -> m

>>= from Monad, bind
(>>=) :: m a -> (a -> m b) -> m b
-}
numerals :: Integer -> Maybe String
numerals n
  | n > 3000 || n <= 0 = Nothing
  | otherwise = (snd <$> x) <> (x >>= (\y -> numerals (n - fst y)))
  where
    x = L.find (\(i, _) -> i <= n) xs
    xs =
      [ (1000, "M"),
        (900, "CM"),
        (500, "D"),
        (400, "CD"),
        (100, "C"),
        (90, "XC"),
        (50, "L"),
        (40, "XL"),
        (10, "X"),
        (9, "IX"),
        (5, "V"),
        (4, "IV"),
        (1, "I")
      ]
