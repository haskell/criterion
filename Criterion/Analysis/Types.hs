{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings,
    RecordWildCards #-}
-- |
-- Module      : Criterion.Analysis.Types
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Analysis types.

module Criterion.Analysis.Types
    (
      Outliers(..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , Regression(..)
    , SampleAnalysis(..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData(rnf))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
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
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON Outliers
instance ToJSON Outliers

instance Binary Outliers where
    put (Outliers v w x y z) = put v >> put w >> put x >> put y >> put z
    get = Outliers <$> get <*> get <*> get <*> get <*> get
instance NFData Outliers

-- | A description of the extent to which outliers in the sample data
-- affect the sample mean and standard deviation.
data OutlierEffect = Unaffected -- ^ Less than 1% effect.
                   | Slight     -- ^ Between 1% and 10%.
                   | Moderate   -- ^ Between 10% and 50%.
                   | Severe     -- ^ Above 50% (i.e. measurements
                                -- are useless).
                     deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance FromJSON OutlierEffect
instance ToJSON OutlierEffect

instance Binary OutlierEffect where
    put Unaffected = putWord8 0
    put Slight     = putWord8 1
    put Moderate   = putWord8 2
    put Severe     = putWord8 3
    get = do
        i <- getWord8
        case i of
            0 -> return Unaffected
            1 -> return Slight
            2 -> return Moderate
            3 -> return Severe
            _ -> fail $ "get for OutlierEffect: unexpected " ++ show i
instance NFData OutlierEffect

instance Monoid Outliers where
    mempty  = Outliers 0 0 0 0 0
    mappend = addOutliers

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)
{-# INLINE addOutliers #-}

-- | Analysis of the extent to which outliers in a sample affect its
-- standard deviation (and to some extent, its mean).
data OutlierVariance = OutlierVariance {
      ovEffect   :: OutlierEffect
    -- ^ Qualitative description of effect.
    , ovDesc     :: String
    -- ^ Brief textual description of effect.
    , ovFraction :: Double
    -- ^ Quantitative description of effect (a fraction between 0 and 1).
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON OutlierVariance
instance ToJSON OutlierVariance

instance Binary OutlierVariance where
    put (OutlierVariance x y z) = put x >> put y >> put z
    get = OutlierVariance <$> get <*> get <*> get

instance NFData OutlierVariance where
    rnf OutlierVariance{..} = rnf ovEffect `seq` rnf ovDesc `seq` rnf ovFraction

data Regression = Regression {
    regPredictors :: [String]
  , regCoeffs     :: Map String Double
  , regRSquare    :: Double
  } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON Regression
instance ToJSON Regression

instance Binary Regression where
    put Regression{..} =
      put regPredictors >> put regCoeffs >> put regRSquare

instance NFData Regression where
    rnf Regression{..} =
      rnf regPredictors `seq` rnf regCoeffs `seq` rnf regRSquare

-- | Result of a bootstrap analysis of a non-parametric sample.
data SampleAnalysis = SampleAnalysis {
      anRegress    :: [Regression]
    , anMean       :: B.Estimate
    , anStdDev     :: B.Estimate
    , anOutlierVar :: OutlierVariance
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON SampleAnalysis
instance ToJSON SampleAnalysis

instance Binary SampleAnalysis where
    put SampleAnalysis{..} =
      put anRegress >> put anMean >> put anStdDev >> put anOutlierVar
    get = SampleAnalysis <$> get <*> get <*> get <*> get

instance NFData SampleAnalysis where
    rnf SampleAnalysis{..} =
        rnf anRegress `seq` rnf anMean `seq` rnf anStdDev `seq` rnf anOutlierVar
