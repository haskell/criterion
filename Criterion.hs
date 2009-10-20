-- |
-- Module      : Criterion
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core benchmarking code.

module Criterion
    (
      Benchmarkable(..)
    , Benchmark
    , bench
    , bgroup
    , runBenchmark
    , runAndAnalyse
    ) where

import Control.Monad ((<=<), forM_, liftM, replicateM_, when)
import Criterion.Analysis (OutlierVariance(..), classifyOutliers,
                           outlierVariance, noteOutliers)
import Criterion.Config (Config(..), Plot(..), fromLJ)
import Criterion.Environment (Environment(..))
import Criterion.IO (note, prolix)
import Criterion.Measurement (getTime, runForAtLeast, secs, time_)
import Criterion.Plot (plotWith, plotKDE, plotTiming)
import Criterion.Types (Benchmarkable(..), Benchmark(..), bench, bgroup)
import Data.Array.Vector ((:*:)(..), lengthU, mapU, foldlU)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (getLast)
import Statistics.Function (createIO)
import Statistics.KernelDensity (epanechnikovPDF)
import Statistics.RandomVariate (withSystemRandom)
import Statistics.Resampling (resample)
import Statistics.Resampling.Bootstrap (Estimate(..), bootstrapBCA)
import Statistics.Sample (mean, stdDev)
import Statistics.Types (Sample)
import System.Mem (performGC)

-- | Run a single benchmark, and return timings measured when
-- executing it.
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

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Benchmarkable b => Config -> Environment -> String -> b
                 -> IO Sample
runAndAnalyseOne cfg env _desc b = do
  times <- runBenchmark cfg env b
  let numSamples = lengthU times
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
  return times
  where bs :: String -> Estimate -> IO ()
        bs d e = note cfg "%s: %s, lb %s, ub %s, ci %.3f\n" d
                   (secs $ estPoint e)
                   (secs $ estLowerBound e) (secs $ estUpperBound e)
                   (estConfidenceLevel e)

plotOne :: Config -> String -> Sample -> IO ()
plotOne cfg desc times = do
  plotWith Timing cfg $ \o -> plotTiming o desc times
  plotWith KernelDensity cfg $ \o -> uncurry (plotKDE o desc Nothing)
                                     (epanechnikovPDF 100 times)

plotAll :: Config -> [(String, Sample)] -> IO ()
plotAll cfg descTimes = forM_ descTimes $ \(desc,times) -> do
  plotWith Timing cfg $ \o -> plotTiming o desc times
  plotWith KernelDensity cfg $ \o -> uncurry (plotKDE o desc extremes)
                                            (epanechnikovPDF 100 times)
  where
    extremes :: Maybe (Double, Double)
    extremes = foldl minMaxMaybe2 Nothing $ mapMaybe (foldlU minMaxMaybe Nothing . snd) descTimes

    minMaxMaybe :: Maybe (Double, Double) -> Double -> Maybe (Double, Double)
    minMaxMaybe a b = minMaxMaybe2 a (b, b)

    minMaxMaybe2 :: Maybe (Double, Double) -> (Double, Double) -> Maybe (Double, Double)
    minMaxMaybe2 Nothing (xMin, xMax) = Just (xMin, xMax)
    minMaxMaybe2 (Just (curMin, curMax)) (xMin, xMax) = Just (min xMin curMin, max xMax curMax)


-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Config
              -> Environment
              -> Benchmark
              -> IO ()
runAndAnalyse p cfg env
  = (if plotSame
     then plotAll cfg
     else const $ return ()) <=< go ""
  where go pfx (Benchmark desc b)
            | p desc'   = do note cfg "\nbenchmarking %s\n" desc'
                             x <- runAndAnalyseOne cfg env desc' b
                             if plotSame
                               then return [(desc', x)]
                               else do plotOne cfg desc' x
                                       return []
            | otherwise = return []
            where desc' = prefix pfx desc
        go pfx (BenchGroup desc bs) = liftM concat $ mapM (go (prefix pfx desc)) bs
        prefix ""  desc = desc
        prefix pfx desc = pfx ++ '/' : desc

        plotSame = fromMaybe False . getLast . cfgPlotSameAxis $ cfg
