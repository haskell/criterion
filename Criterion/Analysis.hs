{-# LANGUAGE BangPatterns, DeriveDataTypeable, RecordWildCards #-}
-- |
-- Module      : Criterion.Analysis
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Analysis code for benchmarks.

module Criterion.Analysis
    (
      Outliers(..)
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
    , resolveAccessors
    , validateAccessors
    , regress
    ) where

import Control.Arrow (second)
import Control.Monad (unless, when)
import Control.Monad.Reader (ask)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Criterion.IO.Printf (note, prolix)
import Criterion.Measurement (secs, threshold)
import Criterion.Monad (Criterion, getGen, getOverhead)
import Criterion.Types
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg)
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (resample)
import Statistics.Sample (mean)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Estimator(..), Sample)
import System.Random.MWC (GenIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Resampling.Bootstrap as B

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = U.foldl' ((. outlier) . mappend) mempty ssa
    where outlier e = Outliers {
                        samplesSeen = 1
                      , lowSevere = if e <= loS && e < hiM then 1 else 0
                      , lowMild = if e > loS && e <= loM then 1 else 0
                      , highMild = if e >= hiM && e < hiS then 1 else 0
                      , highSevere = if e >= hiS && e > loM then 1 else 0
                      }
          !loS = q1 - (iqr * 3)
          !loM = q1 - (iqr * 1.5)
          !hiM = q3 + (iqr * 1.5)
          !hiS = q3 + (iqr * 3)
          q1   = weightedAvg 1 4 ssa
          q3   = weightedAvg 3 4 ssa
          ssa  = sort sa
          iqr  = q3 - q1

-- | Compute the extent to which outliers in the sample data affect
-- the sample mean and standard deviation.
outlierVariance :: B.Estimate  -- ^ Bootstrap estimate of sample mean.
                -> B.Estimate  -- ^ Bootstrap estimate of sample
                               --   standard deviation.
                -> Double      -- ^ Number of original iterations.
                -> OutlierVariance
outlierVariance µ σ a = OutlierVariance effect desc varOutMin
  where
    ( effect, desc ) | varOutMin < 0.01 = (Unaffected, "no")
                     | varOutMin < 0.1  = (Slight,     "slight")
                     | varOutMin < 0.5  = (Moderate,   "moderate")
                     | otherwise        = (Severe,     "severe")
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

-- | Perform an analysis of a measurement.
analyseSample :: Int            -- ^ Experiment number.
              -> String         -- ^ Experiment name.
              -> V.Vector Measured -- ^ Sample data.
              -> ExceptT String Criterion Report
analyseSample i name meas = do
  Config{..} <- ask
  overhead <- lift getOverhead
  let ests      = [Mean,StdDev]
      -- The use of filter here throws away very-low-quality
      -- measurements when bootstrapping the mean and standard
      -- deviations.  Without this, the numbers look nonsensical when
      -- very brief actions are measured.
      stime     = measure (measTime . rescale) .
                  G.filter ((>= threshold) . measTime) . G.map fixTime .
                  G.tail $ meas
      fixTime m = m { measTime = measTime m - overhead / 2 }
      n         = G.length meas
      s         = G.length stime
  _ <- lift $ prolix "bootstrapping with %d of %d samples (%d%%)\n"
              s n ((s * 100) `quot` n)
  gen <- lift getGen
  rs <- mapM (\(ps,r) -> regress gen ps r meas) $
        ((["iters"],"time"):regressions)
  resamps <- liftIO $ resample gen ests resamples stime
  let [estMean,estStdDev] = B.bootstrapBCA confInterval stime ests resamps
      ov = outlierVariance estMean estStdDev (fromIntegral n)
      an = SampleAnalysis {
               anRegress    = rs
             , anOverhead   = overhead
             , anMean       = estMean
             , anStdDev     = estStdDev
             , anOutlierVar = ov
             }
  return Report {
      reportNumber   = i
    , reportName     = name
    , reportKeys     = measureKeys
    , reportMeasured = meas
    , reportAnalysis = an
    , reportOutliers = classifyOutliers stime
    , reportKDEs     = [uncurry (KDE "time") (kde 128 stime)]
    }

-- | Regress the given predictors against the responder.
--
-- Errors may be returned under various circumstances, such as invalid
-- names or lack of needed data.
--
-- See 'olsRegress' for details of the regression performed.
regress :: GenIO
        -> [String]             -- ^ Predictor names.
        -> String               -- ^ Responder name.
        -> V.Vector Measured
        -> ExceptT String Criterion Regression
regress gen predNames respName meas = do
  when (G.null meas) $
    throwE "no measurements"
  accs <- ExceptT . return $ validateAccessors predNames respName
  let unmeasured = [n | (n, Nothing) <- map (second ($ G.head meas)) accs]
  unless (null unmeasured) $
    throwE $ "no data available for " ++ renderNames unmeasured
  let (r:ps)      = map ((`measure` meas) . (fromJust .) . snd) accs
  Config{..} <- ask
  (coeffs,r2) <- liftIO $
                 bootstrapRegress gen resamples confInterval olsRegress ps r
  return Regression {
      regResponder = respName
    , regCoeffs    = Map.fromList (zip (predNames ++ ["y"]) (G.toList coeffs))
    , regRSquare   = r2
    }

singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

-- | Given a list of accessor names (see 'measureKeys'), return either
-- a mapping from accessor name to function or an error message if
-- any names are wrong.
resolveAccessors :: [String]
                 -> Either String [(String, Measured -> Maybe Double)]
resolveAccessors names =
  case unresolved of
    [] -> Right [(n, a) | (n, Just (a,_)) <- accessors]
    _  -> Left $ "unknown metric " ++ renderNames unresolved
  where
    unresolved = [n | (n, Nothing) <- accessors]
    accessors = flip map names $ \n -> (n, Map.lookup n measureAccessors)

-- | Given predictor and responder names, do some basic validation,
-- then hand back the relevant accessors.
validateAccessors :: [String]   -- ^ Predictor names.
                  -> String     -- ^ Responder name.
                  -> Either String [(String, Measured -> Maybe Double)]
validateAccessors predNames respName = do
  when (null predNames) $
    Left "no predictors specified"
  let names = respName:predNames
      dups = map head . filter (not . singleton) .
             List.group . List.sort $ names
  unless (null dups) $
    Left $ "duplicated metric " ++ renderNames dups
  resolveAccessors names

renderNames :: [String] -> String
renderNames = List.intercalate ", " . map show

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
