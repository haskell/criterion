{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
-- |
-- Module      : Criterion.Analysis.Types
-- Copyright   : (c) 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Analysis types.

module Criterion.Analysis.Types
    (
      Outliers (..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , SampleAnalysis(..)
    ) where

import Control.Applicative ((<$>), (<*>), empty, pure)
import Control.DeepSeq (NFData(rnf))
import Data.Aeson.Types
import Data.Data (Data)
import Data.Int (Int64)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import qualified Statistics.Resampling.Bootstrap as B

-- | Outliers from sample data, calculated using the boxplot
-- technique.
data Outliers = Outliers {
      samplesSeen :: {-# UNPACK #-} !Int64
    , lowSevere   :: {-# UNPACK #-} !Int64
    -- ^ More than 3 times the interquartile range (IQR) below the
    -- first quartile.
    , lowMild     :: {-# UNPACK #-} !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: {-# UNPACK #-} !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: {-# UNPACK #-} !Int64
    -- ^ More than 3 times the IQR above the third quartile.
    } deriving (Eq, Read, Show, Typeable, Data)

instance NFData Outliers

instance ToJSON Outliers where
    toJSON Outliers{..} = object [
                            "samplesSeen" .= samplesSeen
                          , "lowSevere" .= lowSevere
                          , "lowMild" .= lowMild
                          , "highMild" .= highMild
                          , "highSevere" .= highSevere
                          ]

instance FromJSON Outliers where
    parseJSON (Object v) = Outliers <$>
                           v .: "samplesSeen" <*>
                           v .: "lowSevere" <*>
                           v .: "lowMild" <*>
                           v .: "highMild" <*>
                           v .: "highSevere"
    parseJSON _ = empty

-- | A description of the extent to which outliers in the sample data
-- affect the sample mean and standard deviation.
data OutlierEffect = Unaffected -- ^ Less than 1% effect.
                   | Slight     -- ^ Between 1% and 10%.
                   | Moderate   -- ^ Between 10% and 50%.
                   | Severe     -- ^ Above 50% (i.e. measurements
                                -- are useless).
                     deriving (Eq, Ord, Read, Show, Typeable, Data)

instance NFData OutlierEffect

instance ToJSON OutlierEffect where
    toJSON Unaffected = "unaffected"
    toJSON Slight     = "slight"
    toJSON Moderate   = "moderate"
    toJSON Severe     = "severe"

instance FromJSON OutlierEffect where
    parseJSON (String t) = case t of
                             _| t== "unaffected" -> pure Unaffected
                             _| t== "slight"     -> pure Slight
                             _| t== "moderate"   -> pure Moderate
                             _| t== "severe"     -> pure Severe
                              | otherwise        -> empty
    parseJSON _ = empty

instance Monoid Outliers where
    mempty  = Outliers 0 0 0 0 0
    mappend = addOutliers

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)
{-# INLINE addOutliers #-}

-- | Analysis of the extent to which outliers in a sample affect its
-- mean and standard deviation.
data OutlierVariance = OutlierVariance {
      ovEffect   :: OutlierEffect
    -- ^ Qualitative description of effect.
    , ovFraction :: Double
    -- ^ Quantitative description of effect (a fraction between 0 and 1).
    } deriving (Eq, Read, Show, Typeable, Data)

instance NFData OutlierVariance where
    rnf OutlierVariance{..} = rnf ovEffect `seq` rnf ovFraction `seq` ()

instance ToJSON OutlierVariance where
    toJSON OutlierVariance{..} = object [
                                   "effect" .= ovEffect
                                 , "fraction" .= ovFraction
                                 ]

instance FromJSON OutlierVariance where
    parseJSON (Object v) = OutlierVariance <$>
                           v .: "effect" <*>
                           v .: "fraction"
    parseJSON _ = empty

-- | Result of a bootstrap analysis of a non-parametric sample.
data SampleAnalysis = SampleAnalysis {
      anMean :: B.Estimate
    , anStdDev :: B.Estimate
    , anOutliers :: OutlierVariance
    } deriving (Eq, Show, Typeable, Data)

instance NFData SampleAnalysis where
    rnf SampleAnalysis{..} =
        rnf anMean `seq` rnf anStdDev `seq` rnf anOutliers `seq` ()

instance ToJSON SampleAnalysis where
    toJSON SampleAnalysis{..} = object [
                                  "mean" .= anMean
                                , "stdDev" .= anStdDev
                                , "outliers" .= anOutliers
                                ]

instance FromJSON SampleAnalysis where
    parseJSON (Object v) = SampleAnalysis <$>
                           v .: "mean" <*>
                           v .: "stdDev" <*>
                           v .: "outliers"
    parseJSON _ = empty
