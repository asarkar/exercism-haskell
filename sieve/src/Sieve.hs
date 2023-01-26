module Sieve (primesUpTo) where

import Control.Monad as M
import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

type MyVec s = ReaderT (MVector s Bool) (ST s)

primesUpTo :: Integer -> [Integer]
primesUpTo n = ST.runST $ do
  let n' = fromInteger n
  v <- VM.replicate (n' + 1) True

  let m = round $ sqrt (fromInteger n :: Float)
  M.forM_ [2 .. m] $ \x -> do
    y <- VM.unsafeRead v (fromInteger x)
    M.when y (R.runReaderT (mark x n) v)

  map toInteger <$> M.filterM (VM.unsafeRead v) [2 .. n']

mark :: Integer -> Integer -> MyVec s ()
mark x n = do
  let xs = [x * x, x * x + x .. n]
  v <- R.ask
  M.forM_ xs $ \i -> do
    VM.unsafeWrite v (fromInteger i) False
