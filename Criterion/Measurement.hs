{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, ScopedTypeVariables,
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
    , measured
    , applyGCStats
    ) where

import Control.Monad (when)
import Criterion.Types (Measured(..))
import Data.Word (Word64)
import GHC.Stats (GCStats(..))
import Text.Printf (printf)
import qualified Control.Exception as Exc
import qualified GHC.Stats as Stats

getGCStats :: IO (Maybe GCStats)
getGCStats =
  (Just `fmap` Stats.getGCStats) `Exc.catch` \(_::Exc.SomeException) ->
  return Nothing

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

applyGCStats :: Maybe GCStats -> Maybe GCStats -> Measured -> Measured
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
