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
import Criterion.Config (Config)
import Criterion.IO (note)
import Criterion.Measurement (secs)
import Data.Array.Vector (foldlU)
import Data.Int (Int64)
import Data.Monoid (Monoid(..))
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Sample (mean)
import Statistics.Types (Sample)

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

data OutlierVariance = Unaffected
                     | Slight
                     | Moderate
                     | Severe
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
classifyOutliers sa = foldlU ((. outlier) . mappend) mempty ssa
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

countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d
{-# INLINE countOutliers #-}

analyseMean :: Config -> Sample -> Int -> IO Double
analyseMean cfg a iters = do
  let µ = mean a
  note cfg "mean is %s (%d iterations)\n" (secs µ) iters
  noteOutliers cfg . classifyOutliers $ a
  return µ

noteOutliers :: Config -> Outliers -> IO ()
noteOutliers cfg o = do
  let frac n = (100::Double) * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int64 -> Double -> String -> IO ()
      check k t d = when (frac k > t) $
                    note cfg "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    note cfg "found %d outliers among %d samples (%.1g%%)\n"
             outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) 0 "low severe"
    check (lowMild o) 1 "low mild"
    check (highMild o) 1 "high mild"
    check (highSevere o) 0 "high severe"
