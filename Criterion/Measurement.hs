{-# LANGUAGE BangPatterns, ForeignFunctionInterface, ScopedTypeVariables,
    TypeOperators #-}

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
    , runForAtLeast
    , getCycles
    , secs
    , time
    , time_
    , cycles
    , getGCStats
    , gcStats
    , diffGCStats
    ) where

import Control.Monad (when)
import Data.Word (Word64)
import GHC.Stats (GCStats(..))
import Text.Printf (printf)
import qualified Control.Exception as Exc
import qualified GHC.Stats as Stats

getGCStats :: IO (Maybe GCStats)
getGCStats =
  (Just `fmap` Stats.getGCStats) `Exc.catch` \(_::Exc.SomeException) ->
  return Nothing

gcStats :: IO a -> IO (Maybe GCStats, a)
gcStats act = do
  start <- getGCStats
  result <- act
  end <- getGCStats
  let delta = do old <- start
                 new <- end
                 return $! diffGCStats old new
  return (delta, result)

-- | A not-very-principled \"difference\" of two sets of GC
-- statistics.
diffGCStats :: GCStats          -- ^ \"old\" stats
            -> GCStats          -- ^ \"new\" stats
            -> GCStats
diffGCStats old new =
  GCStats {
      bytesAllocated = bytesAllocated new - bytesAllocated old
    , numGcs = numGcs new - numGcs old
    , maxBytesUsed = maxBytesUsed new `max` maxBytesUsed old
    , numByteUsageSamples = numByteUsageSamples new `max`
                            numByteUsageSamples old
    , cumulativeBytesUsed = cumulativeBytesUsed new `max`
                            cumulativeBytesUsed old
    , bytesCopied = bytesCopied new - bytesCopied old
    , currentBytesUsed = currentBytesUsed new
    , currentBytesSlop = currentBytesSlop new
    , maxBytesSlop = maxBytesSlop new `max` maxBytesSlop old
    , peakMegabytesAllocated = peakMegabytesAllocated new `max`
                               peakMegabytesAllocated old
    , mutatorCpuSeconds = mutatorCpuSeconds new - mutatorCpuSeconds old
    , mutatorWallSeconds = mutatorWallSeconds new - mutatorWallSeconds old
    , gcCpuSeconds = gcCpuSeconds new - gcCpuSeconds old
    , gcWallSeconds = gcWallSeconds new - gcWallSeconds old
    , cpuSeconds = cpuSeconds new - cpuSeconds old
    , wallSeconds = wallSeconds new - wallSeconds old
    , parTotBytesCopied = parTotBytesCopied new - parTotBytesCopied old
    , parMaxBytesCopied = parMaxBytesCopied new - parMaxBytesCopied old
    }

time :: IO a -> IO (Double, a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  let !delta = end - start
  return (delta, result)

time_ :: IO a -> IO Double
time_ act = do
  start <- getTime
  _ <- act
  end <- getTime
  return $! end - start

cycles :: IO a -> IO (Word64, a)
cycles act = do
  start <- getCycles
  result <- act
  end <- getCycles
  let !delta = end - start
  return (delta, result)

runForAtLeast :: Double -> Int -> (Int -> IO a) -> IO (Double, Int, a)
runForAtLeast howLong initSeed act = loop initSeed (0::Int) =<< getTime
  where
    loop !seed !iters initTime = do
      now <- getTime
      when (now - initTime > howLong * 10) $
        fail (printf "took too long to run: seed %d, iters %d" seed iters)
      (elapsed,result) <- time (act seed)
      if elapsed < howLong
        then loop (seed * 2) (iters+1) initTime
        else return (elapsed, seed, result)

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

foreign import ccall unsafe "criterion_inittime" initializeTime :: IO ()

foreign import ccall unsafe "criterion_rdtsc" getCycles :: IO Word64

-- | Return the current wallclock time, in seconds since some
-- arbitrary time.
--
-- You /must/ call 'initializeTime' once before calling this function!
foreign import ccall unsafe "criterion_gettime" getTime :: IO Double
