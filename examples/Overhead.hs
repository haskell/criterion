-- This benchmark measures the timing overhead added by the various
-- functions we use to measure performance.

{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion.Main
import Criterion.Measurement as M
import GHC.Stats as GHC

main :: IO ()
main = do
  statsEnabled <- getRTSStatsEnabled
  defaultMain $ [
      bench "measure" $            whnfIO (M.measure (whnfIO $ return ()) 1)
    , bench "getTime" $            whnfIO M.getTime
    , bench "getCPUTime" $         whnfIO M.getCPUTime
    , bench "getCycles" $          whnfIO M.getCycles
    , bench "M.getGCStatisticss" $ whnfIO M.getGCStatistics
    ] ++ if statsEnabled
         then [bench
#if MIN_VERSION_base(4,10,0)
                     "GHC.getRTSStats" $ whnfIO GHC.getRTSStats
#else
                     "GHC.getGCStats" $  whnfIO GHC.getGCStats
#endif
              ]
         else []

#if !MIN_VERSION_base(4,6,0)
getRTSStatsEnabled :: IO Bool
getRTSStatsEnabled = return False
#elif !MIN_VERSION_base(4,10,0)
getRTSStatsEnabled :: IO Bool
getRTSStatsEnabled = getGCStatsEnabled
#endif
