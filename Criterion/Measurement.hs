{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

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
    , getCycles
    , getGCStats
    , secs
    , measured
    , applyGCStats
    ) where

import Criterion.Types (Measured(..))
import Data.Word (Word64)
import GHC.Stats (GCStats(..))
import Text.Printf (printf)
import qualified Control.Exception as Exc
import qualified GHC.Stats as Stats

-- | Try to get GC statistics, bearing in mind that the GHC runtime
-- will throw an exception if statistics collection was not enabled
-- using \"@+RTS -T@\".
getGCStats :: IO (Maybe GCStats)
getGCStats =
  (Just `fmap` Stats.getGCStats) `Exc.catch` \(_::Exc.SomeException) ->
  return Nothing

-- | An empty structure.
measured :: Measured
measured = Measured {
      measTime               = 0
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
    | k >= 1e-6  = (k*1e6)  `with` "μs"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
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
