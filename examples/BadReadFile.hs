-- This example demonstrates the peril of trying to benchmark a
-- function that performs lazy I/O.

import Criterion.Main

main :: IO ()
main = defaultMain [
    -- By using whnfIO, when the benchmark loop goes through an
    -- iteration, we inspect only the first constructor returned after
    -- the file is opened.  Since the entire file must be read in
    -- order for it to be closed, this causes file handles to leak,
    -- and our benchmark will probably crash while running with an
    -- error like this:
    --
    -- openFile: resource exhausted (Too many open files)
    bench "whnfIO readFile" $ whnfIO (readFile "BadReadFile.hs")
  ]
