module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

data Clock = Clock Int Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minutes = Clock (normalize x 24) (normalize m 60)
  where
    hr = hour `mod` 24
    (h, m) = divMod minutes 60
    x = (hr + h) `mod` 24
    normalize y base =
      if y < 0
        then base - y
        else y

toString :: Clock -> String
toString (Clock h m) = printf "%s:%s" hr minutes
  where
    hr = printf "%02d" h :: String
    minutes = printf "%02d" m :: String

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minutes (Clock h m) = fromHourMin (hour + h) (minutes + m)
