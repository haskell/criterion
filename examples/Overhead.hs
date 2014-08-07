-- This benchmark measures the timing overhead added by the various
-- functions we use to measure performance.

{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion.Main
import Criterion.Measurement as M
import GHC.Stats as GHC

main :: IO ()
main = do
  statsEnabled <- getGCStatsEnabled
  defaultMain $ [
      bench "measure" $      whnfIO (M.measure (whnfIO $ return ()) 1)
    , bench "getTime" $      whnfIO M.getTime
    , bench "getCPUTime" $   whnfIO M.getCPUTime
    , bench "getCycles" $    whnfIO M.getCycles
    , bench "M.getGCStats" $ whnfIO M.getGCStats
    ] ++ if statsEnabled
         then [bench "GHC.getGCStats" $ whnfIO GHC.getGCStats]
         else []

#if !MIN_VERSION_base(4,6,0)
getGCStatsEnabled :: IO Bool
getGCStatsEnabled = return False
#endif
