module RailFenceCipher (encode, decode) where

import Control.Arrow ((&&&))
import qualified Data.Char as C
import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Ord as O

encode :: Int -> String -> String
encode n xs = concatMap (map snd) zs
  where
    xxs = filter (not . C.isSpace) xs
    ys = zip (indices n) xxs
    zs = L.groupBy ((==) `F.on` fst) $ L.sortBy (O.comparing fst) ys

decode :: Int -> String -> String
decode _ [] = []
decode n xs = L.intercalate "" $ L.unfoldr go rails'
  where
    rails' = rails n xs
    -- Data.List.unsnoc wasn't introduced until base-4.19.0.0.
    unsnoc [] = Nothing
    unsnoc ys = Just ((L.init &&& L.last) ys)

    go [x] = Just (x, [])
    go xxs = do
      (yys, x) <- unsnoc xxs
      let (ys, zzs) = (L.unzip . map (head &&& tail)) yys
      return (ys, x : (reverse . filter (not . null)) zzs)

{-
Generate an infinite list of rail indices.
Example: Given n = 3, output [0,1,2,1,0,1,2, ...]
-}
indices :: Int -> [Int]
indices n = concat $ L.unfoldr go xs
  where
    xs = [0 .. n - 1]
    go ys = Just (init ys, reverse ys)

rails :: Int -> String -> [String]
rails n xs = go xs railLengths
  where
    railLengths = map length $ (L.group . L.sort) $ take (length xs) $ indices n
    go _ [] = []
    go ys (h : t) =
      let (x, y) = L.splitAt h ys
       in x : go y t
