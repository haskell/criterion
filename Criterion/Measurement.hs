{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface,
    ScopedTypeVariables #-}

-- |
-- Module      : Criterion.Measurement
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmark measurement code.

module Criterion.Measurement
    (
      initializeTime
    , getTime
    , getCPUTime
    , getCycles
    , getGCStats
    , secs
    , measure
    , runBenchmark
    , measured
    , applyGCStats
    , threshold
    ) where

import Criterion.Types (Benchmarkable(..), Measured(..))
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Word (Word64)
import GHC.Stats (GCStats(..))
import System.Mem (performGC)
import Text.Printf (printf)
import qualified Control.Exception as Exc
import qualified Data.Vector as V
import qualified GHC.Stats as Stats

-- | Try to get GC statistics, bearing in mind that the GHC runtime
-- will throw an exception if statistics collection was not enabled
-- using \"@+RTS -T@\".
getGCStats :: IO (Maybe GCStats)
getGCStats =
  (Just `fmap` Stats.getGCStats) `Exc.catch` \(_::Exc.SomeException) ->
  return Nothing

-- | Measure the execution of a benchmark a given number of times.
measure :: Benchmarkable        -- ^ Operation to benchmark.
        -> Int64                -- ^ Number of iterations.
        -> IO (Measured, Double)
measure (Benchmarkable run) iters = do
  startStats <- getGCStats
  startTime <- getTime
  startCpuTime <- getCPUTime
  startCycles <- getCycles
  run iters
  endTime <- getTime
  endCpuTime <- getCPUTime
  endCycles <- getCycles
  endStats <- getGCStats
  let !m = applyGCStats endStats startStats $ measured {
             measTime    = max 0 (endTime - startTime)
           , measCpuTime = max 0 (endCpuTime - startCpuTime)
           , measCycles  = max 0 (fromIntegral (endCycles - startCycles))
           , measIters   = iters
           }
  return (m, endTime)
{-# INLINE measure #-}

-- | The amount of time a benchmark must run for in order for us to
-- have some trust in the raw measurement.
--
-- We set this threshold so that we can generate enough data to later
-- perform meaningful statistical analyses.
--
-- The threshold is 30 milliseconds. One use of 'runBenchmark' must
-- accumulate more than 300 milliseconds of total measurements above
-- this threshold before it will finish.
threshold :: Double
threshold = 0.03
{-# INLINE threshold #-}

-- | Run a single benchmark, and return measurements collected while
-- executing it, along with the amount of time the measurement process
-- took.
runBenchmark :: Benchmarkable
             -> Double
             -- ^ Lower bound on how long the benchmarking process
             -- should take.  In practice, this time limit may be
             -- exceeded in order to generate enough data to perform
             -- meaningful statistical analyses.
             -> IO (V.Vector Measured, Double)
runBenchmark bm@(Benchmarkable run) timeLimit = do
  run 1
  start <- performGC >> getTime
  let loop [] !_ !_ _ = error "unpossible!"
      loop (iters:niters) prev count acc = do
        (m, endTime) <- measure bm iters
        let overThresh = max 0 (measTime m - threshold) + prev
        -- We try to honour the time limit, but we also have more
        -- important constraints:
        --
        -- We must generate enough data that bootstrapping won't
        -- simply crash.
        --
        -- We need to generate enough measurements that have long
        -- spans of execution to outweigh the (rather high) cost of
        -- measurement.
        if endTime - start >= timeLimit &&
           overThresh > threshold * 10 &&
           count >= (4 :: Int)
          then do
            let !v = V.reverse (V.fromList acc)
            return (v, endTime - start)
          else loop niters overThresh (count+1) (m:acc)
  loop (squish (unfoldr series 1)) 0 0 []

-- Our series starts its growth very slowly when we begin at 1, so we
-- eliminate repeated values.
squish :: (Eq a) => [a] -> [a]
squish ys = foldr go [] ys
  where go x xs = x : dropWhile (==x) xs

series :: Double -> Maybe (Int64, Double)
series k = Just (truncate l, l)
  where l = k * 1.05

-- | An empty structure.
measured :: Measured
measured = Measured {
      measTime               = 0
    , measCpuTime            = 0
    , measCycles             = 0
    , measIters              = 0

    , measAllocated          = minBound
    , measNumGcs             = minBound
    , measBytesCopied        = minBound
    , measMutatorWallSeconds = bad
    , measMutatorCpuSeconds  = bad
    , measGcWallSeconds      = bad
    , measGcCpuSeconds       = bad
    } where bad = -1/0

-- | Apply the difference between two sets of GC statistics to a
-- measurement.
applyGCStats :: Maybe GCStats
             -- ^ Statistics gathered at the __end__ of a run.
             -> Maybe GCStats
             -- ^ Statistics gathered at the __beginning__ of a run.
             -> Measured
             -- ^ Value to \"modify\".
             -> Measured
applyGCStats (Just end) (Just start) m = m {
    measAllocated          = diff bytesAllocated
  , measNumGcs             = diff numGcs
  , measBytesCopied        = diff bytesCopied
  , measMutatorWallSeconds = diff mutatorWallSeconds
  , measMutatorCpuSeconds  = diff mutatorCpuSeconds
  , measGcWallSeconds      = diff gcWallSeconds
  , measGcCpuSeconds       = diff gcCpuSeconds
  } where diff f = f end - f start
applyGCStats _ _ m = m

-- | Convert a number of seconds to a string.  The string will consist
-- of four decimal places, followed by a short description of the time
-- units.
secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
#ifdef mingw32_HOST_OS
    | k >= 1e-6  = (k*1e6)  `with` "us"
#else
    | k >= 1e-6  = (k*1e6)  `with` "μs"
#endif
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | k >= 1e-15 = (k*1e15) `with` "fs"
    | k >= 1e-18 = (k*1e18) `with` "as"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e3  = printf "%.0f %s" t u
               | t >= 1e2  = printf "%.1f %s" t u
               | t >= 1e1  = printf "%.2f %s" t u
               | otherwise = printf "%.3f %s" t u

-- | Set up time measurement.
foreign import ccall unsafe "criterion_inittime" initializeTime :: IO ()

-- | Read the CPU cycle counter.
foreign import ccall unsafe "criterion_rdtsc" getCycles :: IO Word64

-- | Return the current wallclock time, in seconds since some
-- arbitrary time.
--
-- You /must/ call 'initializeTime' once before calling this function!
foreign import ccall unsafe "criterion_gettime" getTime :: IO Double

-- | Return the amount of elapsed CPU time, combining user and kernel
-- (system) time into a single measure.
foreign import ccall unsafe "criterion_getcputime" getCPUTime :: IO Double
