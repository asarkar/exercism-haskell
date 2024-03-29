module SecretHandshake (handshake) where

import qualified Data.List as L

handshake :: Int -> [String]
handshake n
  | n >= 16 = hs (n - 16)
  | otherwise = (reverse . hs) n

hs :: Int -> [String]
hs n
  | n == 0 = []
  | otherwise = b : hs (n - a)
  where
    xs = [(8, "jump"), (4, "close your eyes"), (2, "double blink"), (1, "wink")]
    x = L.find (\y -> n >= fst y) xs
    (a, b) = case x of
      Just y -> y
      _ -> (0, "")
