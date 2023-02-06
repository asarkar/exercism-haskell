{-# LANGUAGE TupleSections #-}

module Zipper
  ( BinTree (BT),
    fromTree,
    left,
    right,
    setLeft,
    setRight,
    setValue,
    toTree,
    up,
    value,
  )
where

data BinTree a = BT
  { btValue :: a,
    btLeft :: Maybe (BinTree a),
    btRight :: Maybe (BinTree a)
  }
  deriving (Eq, Show)

{-
Heavily inspired by http://learnyouahaskell.com/zippers.
A zipper is a combination of the "focus" node, and a
directed path taken to get to it. A path element consists
of the parent node value, and the subtree _not_ taken. The
path is directed because it remembers the choice made at
each node, i.e. going left or right.
-}
data Crumb a
  = LeftCrumb a (Maybe (BinTree a))
  | RightCrumb a (Maybe (BinTree a))
  deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (BinTree a, Breadcrumbs a)

-- get a zipper out of a tree, the focus is on the root node
fromTree :: BinTree a -> Zipper a
fromTree = (,[])

-- reconstruct the tree out of the zipper
toTree :: Zipper a -> BinTree a
toTree zipper = case up zipper of
  Just parent -> toTree parent
  _ -> fst zipper

-- get the value of the focus node
value :: Zipper a -> a
value (BT x _ _, _) = x

-- move the focus to the left subtree
left :: Zipper a -> Maybe (Zipper a)
left (BT x (Just l) r, bc) = Just (l, LeftCrumb x r : bc)
left _ = Nothing

-- move the focus to the right subtree
right :: Zipper a -> Maybe (Zipper a)
right (BT x l (Just r), bc) = Just (r, RightCrumb x l : bc)
right _ = Nothing

-- move the focus to the parent node
up :: Zipper a -> Maybe (Zipper a)
up (t, LeftCrumb x r : bc) = Just (BT x (Just t) r, bc)
up (t, RightCrumb x l : bc) = Just (BT x l (Just t), bc)
up (_, []) = Nothing

-- set the value of the focus node
setValue :: a -> Zipper a -> Zipper a
setValue x (BT _ l r, bc) = (BT x l r, bc)

-- replace the left subtree of the focus node
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l' (BT x _ r, bc) = (BT x l' r, bc)

-- replace the right subtree of the focus node
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r' (BT x l _, bc) = (BT x l r', bc)
