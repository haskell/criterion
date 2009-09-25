{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators, GADTs #-}

module Criterion
    (
      Benchmarkable(..)
    , Benchmark
    , bench
    , runBenchmark
    , runAndAnalyse
    ) where

import Control.Monad (replicateM_, when)
import Criterion.Analysis
import Criterion.Environment
import Criterion.Measurement
import Criterion.Config
import Criterion.IO
import Criterion.Plot (plotWith)
import Criterion.Types (Benchmarkable(..), Benchmark(..), bench, benchName)
import Data.Array.Vector
import Prelude hiding (catch)
import Statistics.Function (createIO)
import Statistics.Function (indices)
import Statistics.KernelDensity (epanechnikovPDF, fromPoints)
import Statistics.RandomVariate (withSystemRandom)
import Statistics.Resampling (resample)
import Statistics.Resampling.Bootstrap (Estimate(..), bootstrapBCA)
import Statistics.Sample (mean, stdDev)
import Statistics.Types (Sample)
import System.Mem (performGC)

runBenchmark :: Config -> Environment -> Benchmark -> IO Sample
runBenchmark cfg env (Benchmark desc b) = do
  note cfg "\nbenchmarking %s\n" desc
  runForAtLeast 0.1 10000 (`replicateM_` getTime)
  let minTime = envClockResolution env * 1000
  (testTime :*: testIters :*: _) <- runForAtLeast (min minTime 0.1) 1 rpt
  prolix cfg "ran %d iterations in %s\n" testIters (secs testTime)
  let newIters    = ceiling $ minTime * testItersD / testTime
      sampleCount = fromLJ cfgSamples cfg
      newItersD   = fromIntegral newIters
      testItersD  = fromIntegral testIters
  note cfg "collecting %d samples, %d iterations each, in estimated %s\n"
       sampleCount newIters (secs (fromIntegral sampleCount * newItersD * testTime / testItersD))
  times <- fmap (mapU ((/ newItersD) . subtract (envClockCost env))) .
           createIO sampleCount $ \_ -> do
             when (fromLJ cfgPerformGC cfg) $ performGC
             time_ (rpt newIters)
  return times
  where
    rpt k | k <= 0    = return ()
          | otherwise = run b k >> rpt (k-1)

runAndAnalyse :: Config -> Environment -> Benchmark -> IO ()
runAndAnalyse cfg env b = do
  times <- runBenchmark cfg env b
  let numSamples = lengthU times
  analyseMean cfg times numSamples
  plotWith Timing cfg (benchName b ++ " timing") "sample" "time"
           (mapU fromIntegral $ indices times) times
  let (points, pdf) = epanechnikovPDF 100 times
  plotWith KernelDensity cfg (benchName b ++ " kde") "time" "pdf"
           (fromPoints points) pdf
  let ests = [mean,stdDev]
      numResamples = fromLJ cfgResamples cfg
  note cfg "bootstrapping with %d resamples\n" numResamples
  res <- withSystemRandom (\gen -> resample gen ests numResamples times)
  let [em,es] = bootstrapBCA (fromLJ cfgConfInterval cfg) times ests res
      (effect, v) = outlierVariance em es (fromIntegral $ numSamples)
      wibble = case effect of
                 Unaffected -> "unaffected" :: String
                 Slight -> "slightly inflated"
                 Moderate -> "moderately inflated"
                 Severe -> "severely inflated"
  bs "mean" em
  bs "standard deviation" es
  note cfg "variance introduced by outliers: %.3f%%\n" (v * 100)
  note cfg "variance is %s by outliers\n" wibble
  where bs :: String -> Estimate -> IO ()
        bs d e = note cfg "bootstrapped %s: %s, lb %s, ub %s, ci %.3f\n" d
                   (secs $ estPoint e)
                   (secs $ estLowerBound e) (secs $ estUpperBound e)
                   (estConfidenceLevel e)
