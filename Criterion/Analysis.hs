-- |
-- Module      : Criterion.Analysis
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
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
    , OutlierVariance(..)
    , analyseMean
    , countOutliers
    , classifyOutliers
    , noteOutliers
    , outlierVariance
    ) where

import Control.Monad (when)
import Criterion.IO (note)
import Criterion.Measurement (secs)
import Criterion.Monad (Criterion)
import qualified Data.Vector.Unboxed as U
import Data.Int (Int64)
import Data.Monoid (Monoid(..))
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Sample (mean)
import Statistics.Types (Sample)

-- | Outliers from sample data, calculated using the boxplot
-- technique.
data Outliers = Outliers {
      samplesSeen :: {-# UNPACK #-} !Int64
    , lowSevere   :: {-# UNPACK #-} !Int64
    -- ^ More than 3 times the IQR below the first quartile.
    , lowMild     :: {-# UNPACK #-} !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: {-# UNPACK #-} !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: {-# UNPACK #-} !Int64
    -- ^ More than 3 times the IQR above the third quartile.
    } deriving (Eq, Read, Show)

-- | A description of the extent to which outliers in the sample data
-- affect the sample mean and standard deviation.
data OutlierVariance = Unaffected -- ^ Less than 1% effect.
                     | Slight     -- ^ Between 1% and 10%.
                     | Moderate   -- ^ Between 10% and 50%.
                     | Severe     -- ^ Above 50% (i.e. measurements
                                  -- are useless).
                       deriving (Eq, Ord, Show)

instance Monoid Outliers where
    mempty  = Outliers 0 0 0 0 0
    mappend = addOutliers

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)
{-# INLINE addOutliers #-}

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = U.foldl ((. outlier) . mappend) mempty ssa
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
outlierVariance :: Estimate     -- ^ Bootstrap estimate of sample mean.
                -> Estimate     -- ^ Bootstrap estimate of sample
                                --   standard deviation.
                -> Double       -- ^ Number of original iterations.
                -> (OutlierVariance, Double)
outlierVariance µ σ a = (effect, varOutMin)
  where
    effect | varOutMin < 0.01 = Unaffected
           | varOutMin < 0.1  = Slight
           | varOutMin < 0.5  = Moderate
           | otherwise        = Severe
    varOutMin = (minBy varOut 1 (minBy cMax 0 µgMin)) / σb2
    varOut c  = (ac / a) * (σb2 - ac * σg2) where ac = a - c
    σb        = estPoint σ
    µa        = estPoint µ / a
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
        d     = k * 2 where k = µa - x
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
  note "mean is %s (%d iterations)\n" (secs µ) iters
  noteOutliers . classifyOutliers $ a
  return µ

-- | Display a report of the 'Outliers' present in a 'Sample'.
noteOutliers :: Outliers -> Criterion ()
noteOutliers o = do
  let frac n = (100::Double) * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int64 -> Double -> String -> Criterion ()
      check k t d = when (frac k > t) $
                    note "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    note "found %d outliers among %d samples (%.1g%%)\n"
         outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) 0 "low severe"
    check (lowMild o) 1 "low mild"
    check (highMild o) 1 "high mild"
    check (highSevere o) 0 "high severe"
