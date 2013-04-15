{-# LANGUAGE DeriveDataTypeable, RecordWildCards, UnboxedTuples #-}
-- |
-- Module      : Criterion.Analysis
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Analysis code for benchmarks.

module Criterion.Analysis
    (
      Outliers (..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , SampleAnalysis(..)
    , analyseSample
    , scale
    , analyseMean
    , countOutliers
    , classifyOutliers
    , noteOutliers
    , outlierVariance
    ) where

import Control.Monad (when)
import Criterion.Analysis.Types
import Criterion.IO.Printf (note)
import Criterion.Measurement (secs)
import Criterion.Monad (Criterion)
import Data.Int (Int64)
import Data.Monoid (Monoid(..))
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg)
import Statistics.Resampling (Resample, resample)
import Statistics.Sample (mean, stdDev)
import Statistics.Types (Sample)
import System.Random.MWC (withSystemRandom)
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Resampling.Bootstrap as B

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = U.foldl' ((. outlier) . mappend) mempty ssa
    where outlier e = Outliers {
                        samplesSeen = 1
                      , lowSevere = if e <= loS then 1 else 0
                      , lowMild = if e > loS && e <= loM then 1 else 0
                      , highMild = if e >= hiM && e < hiS then 1 else 0
                      , highSevere = if e >= hiS then 1 else 0
                      }
          loS = q1 - (iqr * 3)
          loM = q1 - (iqr * 1.5)
          hiM = q3 + (iqr * 1.5)
          hiS = q3 + (iqr * 3)
          q1  = weightedAvg 1 4 ssa
          q3  = weightedAvg 3 4 ssa
          ssa = sort sa
          iqr = q3 - q1
{-# INLINE classifyOutliers #-}

-- | Compute the extent to which outliers in the sample data affect
-- the sample mean and standard deviation.
outlierVariance :: B.Estimate  -- ^ Bootstrap estimate of sample mean.
                -> B.Estimate  -- ^ Bootstrap estimate of sample
                               --   standard deviation.
                -> Double      -- ^ Number of original iterations.
                -> OutlierVariance
outlierVariance µ σ a = OutlierVariance effect desc varOutMin
  where
    (# effect, desc #) | varOutMin < 0.01 = (# Unaffected, "no" #)
                       | varOutMin < 0.1  = (# Slight,     "slight" #)
                       | varOutMin < 0.5  = (# Moderate,   "moderate" #)
                       | otherwise        = (# Severe,     "severe" #)
    varOutMin = (minBy varOut 1 (minBy cMax 0 µgMin)) / σb2
    varOut c  = (ac / a) * (σb2 - ac * σg2) where ac = a - c
    σb        = B.estPoint σ
    µa        = B.estPoint µ / a
    µgMin     = µa / 2
    σg        = min (µgMin / 4) (σb / sqrt a)
    σg2       = σg * σg
    σb2       = σb * σb
    minBy f q r = min (f q) (f r)
    cMax x    = fromIntegral (floor (-2 * k0 / (k1 + sqrt det)) :: Int)
      where
        k1    = σb2 - a * σg2 + ad
        k0    = -a * ad
        ad    = a * d
        d     = k * k where k = µa - x
        det   = k1 * k1 - 4 * σg2 * k0

-- | Count the total number of outliers in a sample.
countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d
{-# INLINE countOutliers #-}

-- | Display the mean of a 'Sample', and characterise the outliers
-- present in the sample.
analyseMean :: Sample
            -> Int              -- ^ Number of iterations used to
                                -- compute the sample.
            -> Criterion Double
analyseMean a iters = do
  let µ = mean a
  _ <- note "mean is %s (%d iterations)\n" (secs µ) iters
  noteOutliers . classifyOutliers $ a
  return µ

-- | Multiply the 'Estimate's in an analysis by the given value, using
-- 'B.scale'.
scale :: Double                 -- ^ Value to multiply by.
      -> SampleAnalysis -> SampleAnalysis
scale f s@SampleAnalysis{..} = s {
                                 anMean = B.scale f anMean
                               , anStdDev = B.scale f anStdDev
                               }

-- | Perform a bootstrap analysis of a non-parametric sample.
analyseSample :: Double         -- ^ Confidence interval (between 0 and 1).
              -> Sample         -- ^ Sample data.
              -> Int            -- ^ Number of resamples to perform
                                -- when bootstrapping.
              -> IO SampleAnalysis
analyseSample ci samples numResamples = do
  let ests = [mean,stdDev]
  resamples <- withSystemRandom $ \gen ->
               resample gen ests numResamples samples :: IO [Resample]
  let [estMean,estStdDev] = B.bootstrapBCA ci samples ests resamples
      ov = outlierVariance estMean estStdDev (fromIntegral $ U.length samples)
  return SampleAnalysis {
               anMean = estMean
             , anStdDev = estStdDev
             , anOutlierVar = ov
             }

-- | Display a report of the 'Outliers' present in a 'Sample'.
noteOutliers :: Outliers -> Criterion ()
noteOutliers o = do
  let frac n = (100::Double) * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int64 -> Double -> String -> Criterion ()
      check k t d = when (frac k > t) $
                    note "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    _ <- note "found %d outliers among %d samples (%.1g%%)\n"
         outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) 0 "low severe"
    check (lowMild o) 1 "low mild"
    check (highMild o) 1 "high mild"
    check (highSevere o) 0 "high severe"
