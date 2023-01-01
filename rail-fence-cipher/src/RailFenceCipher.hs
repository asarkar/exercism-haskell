module RailFenceCipher (encode, decode) where

import qualified Control.Arrow as A
import qualified Data.Char as C
import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Ord as O

encode :: Int -> String -> String
encode n xs = concatMap (map snd) zs
  where
    xxs = filter (not . C.isSpace) xs
    ys = zip (indices n xxs) xxs
    zs = L.groupBy ((==) `F.on` fst) $ L.sortBy (O.comparing fst) ys

decode :: Int -> String -> String
decode _ [] = []
decode n xs = head firstRail : zigzag (tail firstRail : otherRails)
  where
    ys = rails n xs
    (firstRail, otherRails) = (head A.&&& tail) ys

{-
Generate a list of rail indices corresponding to each character
in the input.
Example: Given n = 3, and xs = "abcde", output [0,1,2,1,0]
-}
indices :: Int -> String -> [Int]
indices n xs = 0 : take x (cycle (ys ++ zs))
  where
    x = length xs - 1
    rng = [0 .. n - 1]
    ys = tail rng
    zs = (tail . reverse) rng

rails :: Int -> String -> [String]
rails n xs = go xs railLengths
  where
    railLengths = map length $ (L.group . L.sort) $ indices n xs
    go _ [] = []
    go ys (h : t) =
      let (x, y) = L.splitAt h ys
       in x : go y t

{-
At each iteration, form a string from the first letters of
the tail of the rails. This string is the decoded text in
the making.
Prepend the head (first rail) to the tails of these tails,
reverse the list and recurse. Stop when all the rails are
exhausted.

Note that for simplicity, this function is invoked with the
first letter of the first rail removed. The caller function
appends it to create the final plaintext.

Examples:

W . . . E . . . C . . . R
. E . R . D . S . O . E . E
. . A . . . I . . . V . . . D

[ECR, ERDSOEE, AIVD] => (EA, [ECR, RDSOEE, IVD])
[IVD, RDSOEE, ECR] => (RE, [IVD, DSOEE, CR])
[CR, DSOEE, IVD] => (DI, [CR, SOEE, VD])
[VD, SOEE, CR] => (SC, [VD, OEE, R])
[R, OEE, VD] => (OV, [R, EE, D])
[D, EE, R] => (ER, [D, E, ''])
[D, E] => (E, [D, ''])
[D] => (D, ['', ''])

Output: EAREDISCOVERD

W . . . . . I . . . . . R
. E . . . D . S . . . E . E
. . A . E . . . C . V . . . D
. . . R . . . . . O . . . . . F

[IR, EDSEE, AECVD, ROF] => (EAR, [IR, DSEE, ECVD, OF])
[OF, ECVD, DSEE, IR] => (EDI, [OF, CVD, SEE, R])
[R, SEE, CVD, OF] => (SCO, [R, EE, VD, F])
[F, VD, EE, R] => (VER, [F, D, E, ''])
[E, D, F] => (EDF, ['','',''])

Output: EAREDISCOVERDF
-}
zigzag :: [String] -> String
zigzag xs
  | null ys = []
  | otherwise = firstCol ++ zigzag zs
  where
    ys = L.dropWhileEnd null xs
    (firstCol, rest) = L.unzip $ map (head A.&&& tail) (tail ys)
    zs = reverse $ head ys : rest
