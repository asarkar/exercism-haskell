module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n
  | n >= 16 = reverse (hs (n - 16))
  | otherwise = hs n

hs :: Int -> [String]
hs n
  | n >= 8 = hs (n - 8) ++ ["jump"]
  | n >= 4 = hs (n - 4) ++ ["close your eyes"]
  | n >= 2 = hs (n - 2) ++ ["double blink"]
  | n >= 1 = hs (n - 1) ++ ["wink"]
  | otherwise = []