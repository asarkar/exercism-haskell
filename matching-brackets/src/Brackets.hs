module Brackets (arePaired) where

import qualified Control.Monad as M
import qualified Data.Maybe as Mb

newtype Stack a = Stack [a] deriving (Show)

newStack :: Stack a
newStack = Stack []

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, newStack)
pop (Stack (x : xs)) = (Just x, Stack xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

arePaired :: String -> Bool
arePaired = go newStack

go :: Stack Char -> String -> Bool
go s [] = isEmpty s
go s (x : xs)
  | x `elem` left = go (push s x) xs
  | x `elem` right = isMatch x && go s' xs
  | otherwise = go s xs
  where
    left = ['(', '{', '[']
    right = [')', '}', ']']
    (top, s') = pop s
    isMatch rt = Mb.isJust $ M.mfilter (== lft) top
      where
        lft = case rt of
          ')' -> '('
          '}' -> '{'
          _ -> '['
