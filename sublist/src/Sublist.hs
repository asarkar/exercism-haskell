-- https://wasp-lang.dev/blog/2021/09/01/haskell-forall-tutorial#forall-and-extension-scopedtypevariables
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Sublist (sublist) where

sublist :: (Ord a) => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | kmp xs ys = Just $ if xs == ys then EQ else LT
  | kmp ys xs = Just GT
  | otherwise = Nothing

-- Taken from https://www.cambridge.org/core/journals/journal-of-functional-programming/article/knuthmorrispratt-illustrated/8EFA77D663D585B68630E372BCE1EBA4
data Tree a = Nil | Node [a] (Tree a) (Tree a) deriving (Show)

kmp :: forall a. (Eq a) => [a] -> [a] -> Bool
kmp needle haystack = any done (scanl step init' haystack)
  where
    make :: [a] -> Tree a -> Tree a
    make [] t = Node [] Nil t
    make ys@(x : xs) t = Node ys next rest
      where
        next = make xs (step t x)
        rest = if check t x then rest' else t
        Node _ _ rest' = t

    init' :: Tree a
    init' = make needle Nil

    step :: Tree a -> a -> Tree a
    step Nil _ = init'
    step acc@(Node _ next rest) x
      | check acc x = next
      | otherwise = step rest x

    check :: Tree a -> a -> Bool
    check (Node (x : _) _ _) y = x == y
    check _ _ = False

    done :: Tree a -> Bool
    done (Node [] _ _) = True
    done _ = False
