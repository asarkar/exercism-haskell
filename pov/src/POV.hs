module POV (fromPOV, tracePathBetween) where

import qualified Control.Monad as M
import qualified Data.Maybe as Mb
import Data.Tree (Tree (Node))
import qualified Data.Tree as T

fromPOV :: (Eq a, Show a) => a -> Tree a -> Maybe (Tree a)
fromPOV = pov Nothing

{-
For example, if we have 0 - 1 - 2, and we are reorienting
on node 2, then node 2 is now the parent of node 1.

Thus, each recursive call creates a node with the value of
the current node, adds the parent node to its children, and
removes the node for which the recursive call is made from
the children.
-}
pov :: (Eq a, Show a) => Maybe (Tree a) -> a -> Tree a -> Maybe (Tree a)
pov parent target (Node val children)
  | val == target = nodeWith children
  | otherwise =
      M.msum $
        map
          (\x -> pov (nodeWith (allBut x)) target x)
          children
  where
    nodeWith xs = Just $ Node val $ p ++ xs
    p = Mb.maybeToList parent
    allBut x = filter (neq x) children
    neq x y = T.rootLabel x /= T.rootLabel y

tracePathBetween :: (Eq a, Show a) => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = fromPOV from tree >>= path to

path :: (Eq a, Show a) => a -> Tree a -> Maybe [a]
path target (Node val children)
  | val == target = Just [val]
  | otherwise = (val :) <$> M.msum xs
  where
    xs = map (path target) children
