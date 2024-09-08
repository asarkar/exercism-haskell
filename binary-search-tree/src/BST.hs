module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

import qualified Data.List as L

-- A binary tree is either empty or it is composed of a root
-- element and two successors, which are binary trees themselves.
data BST a = Empty | Branch a (BST a) (BST a)
  deriving (Show, Eq)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Branch _ left _) = toMaybe left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Branch _ _ right) = toMaybe right

toMaybe :: BST a -> Maybe (BST a)
toMaybe Empty = Nothing
toMaybe tree = Just tree

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Branch v _ _) = Just v

empty :: BST a
empty = Empty

fromList :: (Ord a) => [a] -> BST a
fromList = fromL . L.sort

fromL :: (Ord a) => [a] -> BST a
fromL [] = Empty
fromL xs = Branch (xs !! middle) (fromL left) (fromL (tail right))
  where
    middle = length xs `div` 2
    (left, right) = splitAt middle xs

insert :: (Ord a) => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Branch v left right)
  | x <= v = Branch v (insert x left) right
  | otherwise = Branch v left (insert x right)

singleton :: a -> BST a
singleton x = Branch x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Branch v left right) = toList left ++ [v] ++ toList right
