{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Criterion
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
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
    , Pure
    , nf
    , whnf
    , nfIO
    , whnfIO
    , bench
    , bgroup
    , runBenchmark
    , runAndAnalyse
    ) where

import Control.Monad ((<=<), forM_, replicateM_, when)
import Control.Monad.Trans (liftIO)
import Criterion.Analysis (OutlierEffect(..), OutlierVariance(..),
                           SampleAnalysis(..), analyseSample,
                           classifyOutliers, noteOutliers)
import Criterion.Config (Config(..), Plot(..), Verbosity(..), fromLJ)
import Criterion.Environment (Environment(..))
import Criterion.IO (note, prolix, summary)
import Criterion.Measurement (getTime, runForAtLeast, secs, time_)
import Criterion.Monad (Criterion, getConfig, getConfigItem)
import Criterion.Plot (plotWith, plotKDE, plotTiming)
import Criterion.Report (Report(..), report)
import Criterion.Types (Benchmarkable(..), Benchmark(..), Pure,
                        bench, bgroup, nf, nfIO, whnf, whnfIO)
import qualified Data.Vector.Unboxed as U
import Statistics.Function (minMax)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Types (Sample)
import System.Mem (performGC)
import Text.Printf (printf)

-- | Run a single benchmark, and return timings measured when
-- executing it.
runBenchmark :: Benchmarkable b => Environment -> b -> Criterion Sample
runBenchmark env b = do
  _ <- liftIO $ runForAtLeast 0.1 10000 (`replicateM_` getTime)
  let minTime = envClockResolution env * 1000
  (testTime, testIters, _) <- liftIO $ runForAtLeast (min minTime 0.1) 1 (run b)
  _ <- prolix "ran %d iterations in %s\n" testIters (secs testTime)
  cfg <- getConfig
  let newIters    = ceiling $ minTime * testItersD / testTime
      sampleCount = fromLJ cfgSamples cfg
      newItersD   = fromIntegral newIters
      testItersD  = fromIntegral testIters
      estTime     = (fromIntegral sampleCount * newItersD *
                     testTime / testItersD)
  when (fromLJ cfgVerbosity cfg > Normal || estTime > 5) $
    note "collecting %d samples, %d iterations each, in estimated %s\n"
       sampleCount newIters (secs estTime)
  times <- liftIO . fmap (U.map ((/ newItersD) . subtract (envClockCost env))) .
           U.replicateM sampleCount $ do
             when (fromLJ cfgPerformGC cfg) $ performGC
             time_ (run b newIters)
  return times

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Benchmarkable b => Environment -> String -> b
                 -> Criterion Sample
runAndAnalyseOne env _desc b = do
  times <- runBenchmark env b
  ci <- getConfigItem $ fromLJ cfgConfInterval
  numResamples <- getConfigItem $ fromLJ cfgResamples
  _ <- prolix "analysing with %d resamples\n" numResamples
  SampleAnalysis{..} <- liftIO $ analyseSample ci times numResamples
  let OutlierVariance{..} = anOutliers
  let wibble = case ovEffect of
                 Unaffected -> "unaffected" :: String
                 Slight -> "slightly inflated"
                 Moderate -> "moderately inflated"
                 Severe -> "severely inflated"
  bs "mean" anMean
  summary ","
  bs "std dev" anStdDev
  summary "\n"
  vrb <- getConfigItem $ fromLJ cfgVerbosity
  when (vrb == Verbose || (ovEffect > Unaffected && vrb > Quiet)) $ do
    noteOutliers (classifyOutliers times)
    _ <- note "variance introduced by outliers: %.3f%%\n" (ovFraction * 100)
    _ <- note "variance is %s by outliers\n" wibble
    return ()
  return times
  where bs :: String -> Estimate -> Criterion ()
        bs d e = do _ <- note "%s: %s, lb %s, ub %s, ci %.3f\n" d
                      (secs $ estPoint e)
                      (secs $ estLowerBound e) (secs $ estUpperBound e)
                      (estConfidenceLevel e)
                    summary $ printf "%g,%g,%g" 
                      (estPoint e)
                      (estLowerBound e) (estUpperBound e)

plotAll :: [(String, Sample)] -> Criterion ()
plotAll descTimes = do
  liftIO $ print (map fst descTimes)
  report "foo" (zipWith (\n (d,t) -> Report d n t (kde 128 t)) [0..] descTimes)
  forM_ descTimes $ \(desc,times) -> do
    plotWith Timing $ \o -> plotTiming o desc times
    plotWith KernelDensity $ \o -> uncurry (plotKDE o desc extremes)
                                       (kde 128 times)
    where
      extremes = case descTimes of
                   (_:_:_) -> toJust . minMax . concatU . map snd $ descTimes
                   _       -> Nothing
      concatU = foldr (U.++) U.empty
      toJust r@(lo, hi)
          | lo == infinity || hi == -infinity = Nothing
          | otherwise                         = Just r
          where infinity                      = 1/0

-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Environment
              -> Benchmark
              -> Criterion ()
runAndAnalyse p env = plotAll <=< go ""
  where go pfx (Benchmark desc b)
            | p desc'   = do _ <- note "\nbenchmarking %s\n" desc'
                             summary (show desc' ++ ",") -- String will be quoted
                             x <- runAndAnalyseOne env desc' b
                             return [(desc',x)]
            | otherwise = return []
            where desc' = prefix pfx desc
        go pfx (BenchGroup desc bs) =
            concat `fmap` mapM (go (prefix pfx desc)) bs
        prefix ""  desc = desc
        prefix pfx desc = pfx ++ '/' : desc
