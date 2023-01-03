module LinkedList
  ( LinkedList,
    datum,
    fromList,
    isNil,
    new,
    next,
    nil,
    reverseLinkedList,
    toList,
  )
where

data LinkedList a = Cons a (LinkedList a) | Nil deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil = error "empty list"
datum (Cons a _) = a

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next Nil = Nil
next (Cons _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList l = rev l Nil
  where
    rev Nil a = a
    rev (Cons x xs) a = rev xs (Cons x a)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
