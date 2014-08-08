-- This example demonstrates how to correctly benchmark a function
-- that performs lazy I/O.

import Criterion.Main

main :: IO ()
main = defaultMain [
    -- Because we are using nfIO here, the entire file will be read on
    -- each benchmark loop iteration.  This will cause the associated
    -- file handle to be eagerly closed every time.
    bench "nfIO readFile" $ nfIO (readFile "GoodReadFile.hs")
  ]
