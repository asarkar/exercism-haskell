module IsbnVerifier (isbn) where

import qualified Control.Monad as M
import qualified Data.Char as C

isbn :: String -> Bool
isbn xs = case f xs of
  Left _ -> False
  Right (x, i) -> i == 0 && x `mod` 11 == 0
  where
    -- Good example: https://stackoverflow.com/a/25520325/839733
    f = M.foldM go (0, 10)

go :: (Int, Int) -> Char -> Either () (Int, Int)
go (acc, i) ch
  | C.isDigit ch = Right (acc + i * C.digitToInt ch, i - 1)
  | ch == 'X' && i == 1 = Right (acc + 10, i - 1)
  | ch == '-' = Right (acc, i)
  | otherwise = Left ()
