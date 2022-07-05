module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

expectedMinutesInOven :: Int
expectedMinutesInOven = 40

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes n = n * 2

elapsedTimeInMinutes :: Int -> Int -> Int
elapsedTimeInMinutes x y = y + preparationTimeInMinutes x
