module Triangle (TriangleType (..), triangleType) where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | nonZeroSides && validLengthInequality =
      if equilateral
        then Equilateral
        else
          if isosceles
            then Isosceles
            else Scalene
  | otherwise = Illegal
  where
    nonZeroSides = all (> 0) [a, b, c]
    validLengthInequality = (a + b > c) && (a + c > b) && (b + c > a)
    equilateral = a == b && b == c
    isosceles = a == b || b == c || a == c
