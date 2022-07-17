{-# LANGUAGE NoPatternSynonyms #-}

module WordProblem (answer) where

import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Text.Read as R
import Text.Regex.TDFA

type Trio = (Maybe Integer, String, String)

{-
Megaparsec solutions:
https://exercism.org/tracks/haskell/exercises/wordy/solutions/zhenengxie
https://exercism.org/tracks/haskell/exercises/wordy/solutions/insideoutclub
($>) :: Functor f => f a -> b -> f b 
Replace all locations in the Functor with the same value.
(*>) :: Applicative f => f a -> f b -> f b
Sequence actions, discarding the value of the first argument.
(<*) :: Applicative f => f a -> f b -> f a
Sequence actions, discarding the value of the second argument.
-}
answer :: String -> Maybe Integer
answer problem = eval n s
  where
    (n, _, s) = parse problem

eval :: Maybe Integer -> String -> Maybe Integer
eval x xs
  | M.isNothing x = x
  | M.isNothing y && xs == "?" = x
  | otherwise = eval n s
  where
    (y, op, s) = parse xs
    n = case op of
      "plus" -> (+) <$> x <*> y
      "minus" -> (-) <$> x <*> y
      "multiplied by" -> (*) <$> x <*> y
      "divided by" | y /= Just 0 -> div <$> x <*> y
      "raised to the" -> (^) <$> x <*> y
      _ -> Nothing

parse :: String -> Trio
parse text = (n, op, s)
  where
    pattern = "[-]?[[:digit:]]+"
    (prefix, num, suffix) = text =~ pattern :: (String, String, String)
    trim = T.unpack . T.strip . T.pack
    n = R.readMaybe $ trim num
    op = trim prefix
    s = trim suffix