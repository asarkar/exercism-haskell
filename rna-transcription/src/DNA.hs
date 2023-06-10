module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right ""
toRNA [x]
  | x == 'G' = Right "C"
  | x == 'C' = Right "G"
  | x == 'T' = Right "A"
  | x == 'A' = Right "U"
  | otherwise = Left x
{-
https://stackoverflow.com/a/6280709/839733
http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors

<$> takes a function taking an 'a' and returning a 'b',
and a functor that contains an 'a', and it returns a
functor that contains a 'b'. '(++) <$> toRNA [x]'
creates a function wrapped in an Either.

<*> takes a functor that contains a function taking an
'a' and returning a 'b', and a functor that contains an 'a',
and it returns a functor that contains a 'b'.
'(++)(toRNA [x])' is applied to 'toRNA xs' using <*>.
A 'Left' short circuits the joining.
-}
toRNA (x : xs) = (++) <$> toRNA [x] <*> toRNA xs
