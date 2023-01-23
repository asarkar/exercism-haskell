{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import qualified Control.Concurrent as C
import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Frequency (frequency)
import qualified System.Directory as D
import qualified System.Environment as E
import qualified System.FilePath as FP
import qualified Text.Read as R

{-
Execute with:
'stack --resolver lts --work-dir . \
--stack-yaml parallel-letter-frequency/stack.yaml \
run -- [--file <f>] [--workers <n>]'

Processing 'bible.txt' on 10 cores using 5 workers..

   1,417,042,168 bytes allocated in the heap
     125,122,160 bytes copied during GC
      28,160,096 bytes maximum residency (5 sample(s))
         937,888 bytes maximum slop
             113 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       189 colls,   189 par    0.087s   0.058s     0.0003s    0.0019s
  Gen  1         5 colls,     4 par    0.044s   0.013s     0.0026s    0.0052s

  Parallel GC work balance: 36.86% (serial 0%, perfect 100%)

  TASKS: 22 (1 bound, 21 peak workers (21 total), using -N10)

  SPARKS: 61 (31 converted, 0 overflowed, 0 dud, 30 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.010s elapsed)
  MUT     time    0.651s  (  0.357s elapsed)
  GC      time    0.131s  (  0.071s elapsed)
  EXIT    time    0.000s  (  0.012s elapsed)
  Total   time    0.783s  (  0.450s elapsed)

  Alloc rate    2,176,010,798 bytes per MUT second

  Productivity  83.2% of total user, 79.4% of total elapsed
-}
main :: IO ()
main = do
  cores <- C.getNumCapabilities
  args <- E.getArgs
  let numWorkers =
        Mb.fromMaybe (cores `div` 2) $
          readArg "--workers" args >>= R.readMaybe
  let file = Mb.fromMaybe "alice.txt" $ readArg "--file" args
  dataDir <- E.getExecutablePath >>= D.canonicalizePath >>= getDataDir
  texts <- T.lines <$> (TIO.readFile $ dataDir FP.</> file)
  putStrLn $
    "\nProcessing '"
      ++ file
      ++ "' on "
      ++ show cores
      ++ " cores using "
      ++ show numWorkers
      ++ " workers..\n"
  -- strict match, else no sparks are created
  let !_ = frequency numWorkers texts
  return ()
  where
    getDataDir :: FilePath -> IO FilePath
    getDataDir path = do
      let dataDir = path FP.</> "app" FP.</> "data"
      exists <- D.doesDirectoryExist dataDir
      if exists
        then pure dataDir
        else
          if FP.takeDirectory path == path
            then error "data directory not found"
            else getDataDir $ FP.takeDirectory path

    readArg :: String -> [String] -> Maybe String
    readArg name args = do
      i <- L.elemIndex name args
      M.guard (i < length args - 1)
      return (args L.!! (i + 1))
