module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import qualified Control.Monad as M
import qualified Data.Char as C
import qualified System.Random.Stateful as RS

caesarDecode :: String -> String -> String
caesarDecode key encodedText = xcode key encodedText (-)

caesarEncode :: String -> String -> String
caesarEncode key text = xcode key text (+)

xcode :: String -> String -> (Int -> Int -> Int) -> String
xcode k s f = zipWith go key s
  where
    key = take (length s) $ cycle k
    ixA = C.ord 'a'
    toI x = C.ord x - ixA
    go x y =
      let offset = toI x
          c = f (toI y) offset `mod` 26
       in C.chr $ c + ixA

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = fmap (\k -> (k, caesarEncode k text)) key
  where
    key = M.replicateM 101 (RS.uniformRM ('a', 'z') RS.globalStdGen)
