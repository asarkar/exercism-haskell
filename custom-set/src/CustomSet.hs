{-# LANGUAGE DeriveFoldable #-}

module CustomSet
  ( delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
  )
where

import qualified Data.Foldable as F
import qualified Data.Function as Fn
import Prelude hiding (null)

data CustomSet a
  = Nil
  | Node (CustomSet a) a (CustomSet a)
  deriving (Show, Foldable)

instance (Eq a) => Eq (CustomSet a) where
  (==) = (==) `Fn.on` toList

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete _ Nil = Nil
delete x (Node l y r) = case compare x y of
  LT -> Node (delete x l) y r
  EQ -> l `union` r
  GT -> Node l y (delete x r)

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference = foldr delete

empty :: CustomSet a
empty = Nil

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldr insert empty

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x Nil = Node Nil x Nil
insert x n@(Node l y r) = case compare x y of
  LT -> Node (insert x l) y r
  EQ -> n
  GT -> Node l y (insert x r)

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA = foldr (\x -> if member x setA then insert x else id) empty

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
{-
composition (.) is for functions of single arguments, but intersection
takes two, so, (null .) . intersection

If intersection required three arguments, then ((null .) .) . intersection

Check at https://pointfree.io. It takes a lambda with all the arguments
present, like \ a b c -> f (g a b c), and it produces ((f .) .) . g

In general, when we need to compose a unary function 'f' with an n-ary 'g'
function, add ( .) for each n around 'f'.

https://www.reddit.com/r/haskell/comments/13u23yz/custom_set_implementation/
-}
isDisjointFrom = (null .) . intersection

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (`member` setB) setA

member :: (Ord a) => a -> CustomSet a -> Bool
member _ Nil = False
member x (Node l y r) = case compare x y of
  LT -> member x l
  EQ -> True
  GT -> member x r

null :: CustomSet a -> Bool
null Nil = True
null _ = False

size :: CustomSet a -> Int
size = length

toList :: CustomSet a -> [a]
toList = F.toList

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union = foldr insert
