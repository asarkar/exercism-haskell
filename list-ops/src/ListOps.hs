module ListOps
  ( length,
    reverse,
    map,
    filter,
    foldr,
    foldl',
    (++),
    concat,
  )
where

import Prelude hiding
  ( concat,
    filter,
    foldr,
    length,
    map,
    reverse,
    (++),
  )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x : xs) = let z' = f z x in z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

length :: [a] -> Int
length = foldr (\_ y -> y + 1) 0

reverse :: [a] -> [a]
-- reverse xs = foldr (\x g -> g . (x :)) id xs []
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x y -> if p x then x : y else y) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
