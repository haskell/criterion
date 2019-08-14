-- This benchmark measures the timing overhead added by the various
-- functions we use to measure performance.

{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion.Main
import Criterion.Measurement as M
import GHC.Stats as GHC

main :: IO ()
main = do
  M.initializeTime -- Need to do this before calling M.getTime
  statsEnabled <- getRTSStatsEnabled'
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

-- On GHCJS:
-- uncaught exception in Haskell main thread: ReferenceError: h$getRTSStatsEnabled is not defined
-- ReferenceError: h$getRTSStatsEnabled is not defined
getRTSStatsEnabled' :: IO Bool
#if defined(__GHCJS__) || !MIN_VERSION_base(4,6,0)
getRTSStatsEnabled' = return False
#elif !MIN_VERSION_base(4,10,0)
getRTSStatsEnabled' = getGCStatsEnabled
#else
getRTSStatsEnabled' = getRTSStatsEnabled
#endif
