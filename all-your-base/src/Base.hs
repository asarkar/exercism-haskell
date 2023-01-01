module Base (Error (..), rebase) where

import qualified Data.List as L
import qualified Data.Tuple as T

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
  deriving (Show, Eq)

{-
1. Convert to decimal using Horner's method (https://en.wikipedia.org/wiki/Horner%27s_method).
2. Convert to the target base.
-}
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise = case decimal inputBase (reverse inputDigits) of
      Right dec -> (Right . reverse) $ L.unfoldr go dec
      Left err -> Left err
  where
    go x
      | x > 0 = (Just . T.swap) $ divMod x outputBase
      | otherwise = Nothing

decimal :: Integral a => a -> [a] -> Either (Error a) a
decimal _ [] = Right 0
decimal base (x : xs)
  | x < 0 || x >= base = Left $ InvalidDigit x
  | otherwise = (\i -> x + base * i) <$> decimal base xs
