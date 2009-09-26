module Criterion
    (
      Benchmarkable(..)
    , Benchmark
    , bench
    , bgroup
    , runBenchmark
    , runAndAnalyse
    ) where

import Control.Monad (replicateM_, when)
import Criterion.Analysis (OutlierVariance(..), classifyOutliers,
                           outlierVariance, noteOutliers)
import Criterion.Config (Config(..), Plot(..), fromLJ)
import Criterion.Environment (Environment(..))
import Criterion.IO (note, prolix)
import Criterion.Measurement (getTime, runForAtLeast, secs, time_)
import Criterion.Plot (plotWith, plotKDE, plotTiming)
import Criterion.Types (Benchmarkable(..), Benchmark(..), bench, bgroup)
import Data.Array.Vector ((:*:)(..), lengthU, mapU)
import Prelude hiding (catch)
import Statistics.Function (createIO)
import Statistics.KernelDensity (epanechnikovPDF)
import Statistics.RandomVariate (withSystemRandom)
import Statistics.Resampling (resample)
import Statistics.Resampling.Bootstrap (Estimate(..), bootstrapBCA)
import Statistics.Sample (mean, stdDev)
import Statistics.Types (Sample)
import System.Mem (performGC)

runBenchmark :: Benchmarkable b => Config -> Environment -> b -> IO Sample
runBenchmark cfg env b = do
  runForAtLeast 0.1 10000 (`replicateM_` getTime)
  let minTime = envClockResolution env * 1000
  (testTime :*: testIters :*: _) <- runForAtLeast (min minTime 0.1) 1 timeLoop
  prolix cfg "ran %d iterations in %s\n" testIters (secs testTime)
  let newIters    = ceiling $ minTime * testItersD / testTime
      sampleCount = fromLJ cfgSamples cfg
      newItersD   = fromIntegral newIters
      testItersD  = fromIntegral testIters
  note cfg "collecting %d samples, %d iterations each, in estimated %s\n"
       sampleCount newIters (secs (fromIntegral sampleCount * newItersD *
                                   testTime / testItersD))
  times <- fmap (mapU ((/ newItersD) . subtract (envClockCost env))) .
           createIO sampleCount . const $ do
             when (fromLJ cfgPerformGC cfg) $ performGC
             time_ (timeLoop newIters)
  return times
  where
    timeLoop k | k <= 0    = return ()
               | otherwise = run b k >> timeLoop (k-1)

runAndAnalyseOne :: Benchmarkable b => Config -> Environment -> String -> b
                 -> IO ()
runAndAnalyseOne cfg env desc b = do
  times <- runBenchmark cfg env b
  let numSamples = lengthU times
  plotWith Timing cfg $ \o -> plotTiming o desc times
  plotWith KernelDensity cfg $ \o -> uncurry (plotKDE o desc)
                                     (epanechnikovPDF 100 times)
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
  bs "std dev" es
  noteOutliers cfg (classifyOutliers times)
  note cfg "variance introduced by outliers: %.3f%%\n" (v * 100)
  note cfg "variance is %s by outliers\n" wibble
  where bs :: String -> Estimate -> IO ()
        bs d e = note cfg "%s: %s, lb %s, ub %s, ci %.3f\n" d
                   (secs $ estPoint e)
                   (secs $ estLowerBound e) (secs $ estUpperBound e)
                   (estConfidenceLevel e)

runAndAnalyse :: (String -> Bool) -> Config -> Environment -> Benchmark -> IO ()
runAndAnalyse p cfg env (Benchmark desc b)
    | p desc    = do note cfg "\nbenchmarking %s\n" desc
                     runAndAnalyseOne cfg env desc b
    | otherwise = return ()
runAndAnalyse p cfg env (BenchGroup desc bs) =
    mapM_ (runAndAnalyse p' cfg env) bs
  where p' | p desc    = const True
           | otherwise = p
