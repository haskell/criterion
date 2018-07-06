{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      : Criterion.Types
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for benchmarking.
--
-- The core type is 'Benchmarkable', which admits both pure functions
-- and 'IO' actions.
--
-- For a pure function of type @a -> b@, the benchmarking harness
-- calls this function repeatedly, each time with a different 'Int64'
-- argument (the number of times to run the function in a loop), and
-- reduces the result the function returns to weak head normal form.
--
-- For an action of type @IO a@, the benchmarking harness calls the
-- action repeatedly, but does not reduce the result.

module Criterion.Types
    (
    -- * Configuration
      Config(..)
    , Verbosity(..)
    -- * Benchmark descriptions
    , Benchmarkable(..)
    , Benchmark(..)
    -- * Measurements
    , Measured(..)
    , fromInt
    , toInt
    , fromDouble
    , toDouble
    , measureAccessors
    , measureKeys
    , measure
    , rescale
    -- * Benchmark construction
    , env
    , envWithCleanup
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup
    , toBenchmarkable
    , bench
    , bgroup
    , addPrefix
    , benchNames
    -- ** Evaluation control
    , nf
    , whnf
    , nfIO
    , whnfIO
    , nfAppIO
    , whnfAppIO
    -- * Result types
    , Outliers(..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , Regression(..)
    , KDE(..)
    , Report(..)
    , SampleAnalysis(..)
    , DataRecord(..)
    ) where

import Control.DeepSeq (NFData(rnf))
import Criterion.Measurement.Types
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map)
import GHC.Generics (Generic)
import Prelude ()
import Prelude.Compat
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Types as St
import           Statistics.Resampling.Bootstrap ()

-- | Control the amount of information displayed.
data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                           Generic)

-- | Top-level benchmarking configuration.
data Config = Config {
      confInterval :: St.CL Double
      -- ^ Confidence interval for bootstrap estimation (greater than
      -- 0, less than 1).
    , timeLimit    :: Double
      -- ^ Number of seconds to run a single benchmark.  (In practice,
      -- execution time will very slightly exceed this limit.)
    , resamples    :: Int
      -- ^ Number of resamples to perform when bootstrapping.
    , regressions  :: [([String], String)]
      -- ^ Regressions to perform.
    , rawDataFile  :: Maybe FilePath
      -- ^ File to write binary measurement and analysis data to.  If
      -- not specified, this will be a temporary file.
    , reportFile   :: Maybe FilePath
      -- ^ File to write report output to, with template expanded.
    , csvFile      :: Maybe FilePath
      -- ^ File to write CSV summary to.
    , jsonFile     :: Maybe FilePath
      -- ^ File to write JSON-formatted results to.
    , junitFile    :: Maybe FilePath
      -- ^ File to write JUnit-compatible XML results to.
    , verbosity    :: Verbosity
      -- ^ Verbosity level to use when running and analysing
      -- benchmarks.
    , template     :: FilePath
      -- ^ Template file to use if writing a report.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)


-- | Outliers from sample data, calculated using the boxplot
-- technique.
data Outliers = Outliers {
      samplesSeen :: !Int64
    , lowSevere   :: !Int64
    -- ^ More than 3 times the interquartile range (IQR) below the
    -- first quartile.
    , lowMild     :: !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: !Int64
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

instance Semigroup Outliers where
    (<>) = addOutliers

instance Monoid Outliers where
    mempty  = Outliers 0 0 0 0 0
#if !(MIN_VERSION_base(4,11,0))
    mappend = addOutliers
#endif

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

-- | Results of a linear regression.
data Regression = Regression {
    regResponder  :: String
    -- ^ Name of the responding variable.
  , regCoeffs     :: Map String (St.Estimate St.ConfInt Double)
    -- ^ Map from name to value of predictor coefficients.
  , regRSquare    :: St.Estimate St.ConfInt Double
    -- ^ R&#0178; goodness-of-fit estimate.
  } deriving (Eq, Read, Show, Typeable, Generic)

instance FromJSON Regression
instance ToJSON Regression

instance Binary Regression where
    put Regression{..} =
      put regResponder >> put regCoeffs >> put regRSquare
    get = Regression <$> get <*> get <*> get

instance NFData Regression where
    rnf Regression{..} =
      rnf regResponder `seq` rnf regCoeffs `seq` rnf regRSquare

-- | Result of a bootstrap analysis of a non-parametric sample.
data SampleAnalysis = SampleAnalysis {
      anRegress    :: [Regression]
      -- ^ Estimates calculated via linear regression.
    , anMean       :: St.Estimate St.ConfInt Double
      -- ^ Estimated mean.
    , anStdDev     :: St.Estimate St.ConfInt Double
      -- ^ Estimated standard deviation.
    , anOutlierVar :: OutlierVariance
      -- ^ Description of the effects of outliers on the estimated
      -- variance.
    } deriving (Eq, Read, Show, Typeable, Generic)

instance FromJSON SampleAnalysis
instance ToJSON SampleAnalysis

instance Binary SampleAnalysis where
    put SampleAnalysis{..} = do
      put anRegress; put anMean; put anStdDev; put anOutlierVar
    get = SampleAnalysis <$> get <*> get <*> get <*> get

instance NFData SampleAnalysis where
    rnf SampleAnalysis{..} =
        rnf anRegress `seq` rnf anMean `seq`
        rnf anStdDev `seq` rnf anOutlierVar

-- | Data for a KDE chart of performance.
data KDE = KDE {
      kdeType   :: String
    , kdeValues :: U.Vector Double
    , kdePDF    :: U.Vector Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON KDE
instance ToJSON KDE

instance Binary KDE where
    put KDE{..} = put kdeType >> put kdeValues >> put kdePDF
    get = KDE <$> get <*> get <*> get

instance NFData KDE where
    rnf KDE{..} = rnf kdeType `seq` rnf kdeValues `seq` rnf kdePDF

-- | Report of a sample analysis.
data Report = Report {
      reportNumber   :: Int
      -- ^ A simple index indicating that this is the /n/th report.
    , reportName     :: String
      -- ^ The name of this report.
    , reportKeys     :: [String]
      -- ^ See 'measureKeys'.
    , reportMeasured :: V.Vector Measured
      -- ^ Raw measurements.
    , reportAnalysis :: SampleAnalysis
      -- ^ Report analysis.
    , reportOutliers :: Outliers
      -- ^ Analysis of outliers.
    , reportKDEs     :: [KDE]
      -- ^ Data for a KDE of times.
    } deriving (Eq, Read, Show, Typeable, Generic)

instance FromJSON Report
instance ToJSON Report

instance Binary Report where
    put Report{..} =
      put reportNumber >> put reportName >> put reportKeys >>
      put reportMeasured >> put reportAnalysis >> put reportOutliers >>
      put reportKDEs

    get = Report <$> get <*> get <*> get <*> get <*> get <*> get <*> get

instance NFData Report where
    rnf Report{..} =
      rnf reportNumber `seq` rnf reportName `seq` rnf reportKeys `seq`
      rnf reportMeasured `seq` rnf reportAnalysis `seq` rnf reportOutliers `seq`
      rnf reportKDEs

data DataRecord = Measurement Int String (V.Vector Measured)
                | Analysed Report
                deriving (Eq, Read, Show, Typeable, Generic)

instance Binary DataRecord where
  put (Measurement i n v) = putWord8 0 >> put i >> put n >> put v
  put (Analysed r)        = putWord8 1 >> put r

  get = do
    w <- getWord8
    case w of
      0 -> Measurement <$> get <*> get <*> get
      1 -> Analysed    <$> get
      _ -> error ("bad tag " ++ show w)

instance NFData DataRecord where
  rnf (Measurement i n v) = rnf i `seq` rnf n `seq` rnf v
  rnf (Analysed r)        = rnf r

instance FromJSON DataRecord
instance ToJSON DataRecord
