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
-- The core class is 'Benchmarkable', which admits both pure functions
-- and 'IO' actions.
--
-- For a pure function of type @a -> b@, the benchmarking harness
-- calls this function repeatedly, each time with a different 'Int'
-- argument, and reduces the result the function returns to weak head
-- normal form.  If you need the result reduced to normal form, that
-- is your responsibility.
--
-- For an action of type @IO a@, the benchmarking harness calls the
-- action repeatedly, but does not reduce the result.

module Criterion.Types
    (
    -- * Benchmark descriptions
      Benchmarkable(..)
    , Benchmark(..)
    , Measured(..)
    , measureNames
    , fromInt
    , toInt
    , fromDouble
    , toDouble
    , measure
    , rescale
    , whnf
    , nf
    , nfIO
    , whnfIO
    , bench
    , bgroup
    , benchNames
    -- * Result types
    , Outliers(..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , Regression(..)
    , KDE(..)
    , Report(..)
    , SampleAnalysis(..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData(rnf))
import Control.Exception (evaluate)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Resampling.Bootstrap as B

-- | A pure function or impure action that can be benchmarked. The
-- 'Int64' parameter indicates the number of times to run the given
-- function or action.
newtype Benchmarkable = Benchmarkable (Int64 -> IO ())

-- | A collection of measurements made while benchmarking.
data Measured = Measured {
      measTime               :: !Double
    , measCycles             :: !Int64
    , measIters              :: !Int64

    -- GC statistics are only available if a benchmark was run with
    -- "+RTS -T".  If not available, they're set to huge negative
    -- values.
    , measAllocated          :: !Int64
    , measNumGcs             :: !Int64
    , measBytesCopied        :: !Int64
    , measMutatorWallSeconds :: !Double
    , measMutatorCpuSeconds  :: !Double
    , measGcWallSeconds      :: !Double
    , measGcCpuSeconds       :: !Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance FromJSON Measured where
    parseJSON v = do
      (a,b,c,d,e,f,g,h,i,j) <- parseJSON v
      return $ Measured a b c d e f g h i j

instance ToJSON Measured where
    toJSON Measured{..} = toJSON
      (measTime, measCycles, measIters,
       i measAllocated, i measNumGcs, i measBytesCopied,
       d measMutatorWallSeconds, d measMutatorCpuSeconds,
       d measGcWallSeconds, d measMutatorCpuSeconds)
      where i = fromInt; d = fromDouble

instance NFData Measured where
    rnf Measured{} = ()

measureNames :: [String]
measureNames = ["time", "cycles", "iters", "allocated", "numGcs", "bytesCopied",
                "mutatorWallSeconds", "mutatorCpuSeconds", "gcWallSeconds",
                "gcCpuSeconds"]

rescale :: Measured -> Measured
rescale m@Measured{..} = m {
      measTime               = d measTime
    , measCycles             = i measCycles
    -- skip measIters
    , measNumGcs             = i measNumGcs
    , measBytesCopied        = i measBytesCopied
    , measMutatorWallSeconds = d measMutatorWallSeconds
    , measMutatorCpuSeconds  = d measMutatorCpuSeconds
    , measGcWallSeconds      = d measGcWallSeconds
    , measGcCpuSeconds       = d measGcCpuSeconds
    } where
        d k = maybe k (/ iters) (fromDouble k)
        i k = maybe k (round . (/ iters)) (fromIntegral <$> fromInt k)
        iters               = fromIntegral measIters :: Double

fromInt :: Int64 -> Maybe Int64
fromInt i | i == minBound = Nothing
          | otherwise     = Just i

toInt :: Maybe Int64 -> Int64
toInt Nothing  = minBound
toInt (Just i) = i

fromDouble :: Double -> Maybe Double
fromDouble d | isInfinite d || isNaN d = Nothing
             | otherwise               = Just d

toDouble :: Maybe Double -> Double
toDouble Nothing  = -1/0
toDouble (Just d) = d

instance Binary Measured where
    put Measured{..} = do
      put measTime; put measCycles; put measIters
      put measAllocated; put measNumGcs; put measBytesCopied
      put measMutatorWallSeconds; put measMutatorCpuSeconds
      put measGcWallSeconds; put measGcCpuSeconds
    get = Measured <$> get <*> get <*> get
                   <*> get <*> get <*> get <*> get <*> get <*> get <*> get

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable
whnf = pureFunc id
{-# INLINE whnf #-}

-- | Apply an argument to a function, and evaluate the result to head
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = pureFunc rnf
{-# INLINE nf #-}

pureFunc :: (b -> c) -> (a -> b) -> a -> Benchmarkable
pureFunc reduce f0 x0 = Benchmarkable $ go f0 x0
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)
{-# INLINE pureFunc #-}

-- | Perform an action, then evaluate its result to head normal form.
-- This is particularly useful for forcing a lazy IO action to be
-- completely performed.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = impure rnf
{-# INLINE nfIO #-}

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).  This is useful for forcing an IO action whose result
-- is an expression to be evaluated down to a more useful value.
whnfIO :: IO a -> Benchmarkable
whnfIO = impure id
{-# INLINE whnfIO #-}

impure :: (a -> b) -> IO a -> Benchmarkable
impure strategy a = Benchmarkable go
  where go n
          | n <= 0    = return ()
          | otherwise = a >>= (evaluate . strategy) >> go (n-1)
{-# INLINE impure #-}

-- | A benchmark may consist of either a single 'Benchmarkable' item
-- with a name, created with 'bench', or a (possibly nested) group of
-- 'Benchmark's, created with 'bgroup'.
data Benchmark where
    Benchmark    :: String -> Benchmarkable -> Benchmark
    BenchGroup   :: String -> [Benchmark] -> Benchmark

-- | Create a single benchmark.
bench :: String                 -- ^ A name to identify the benchmark.
      -> Benchmarkable
      -> Benchmark
bench = Benchmark

-- | Group several benchmarks together under a common name.
bgroup :: String                -- ^ A name to identify the group of benchmarks.
       -> [Benchmark]           -- ^ Benchmarks to group under this name.
       -> Benchmark
bgroup = BenchGroup

-- | Retrieve the names of all benchmarks.  Grouped benchmarks are
-- prefixed with the name of the group they're in.
benchNames :: Benchmark -> [String]
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = map ((d ++ "/") ++) . concatMap benchNames $ bs

instance Show Benchmark where
    show (Benchmark d _)  = ("Benchmark " ++ show d)
    show (BenchGroup d _) = ("BenchGroup " ++ show d)

measure :: (U.Unbox a) => (Measured -> a) -> V.Vector Measured -> U.Vector a
measure f v = U.convert . V.map f $ v

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

data Report = Report {
      reportNumber   :: Int
    , reportName     :: String
    , reportKeys     :: [String]
    , reportMeasured :: V.Vector Measured
    , reportAnalysis :: SampleAnalysis
    , reportOutliers :: Outliers
    , reportKDEs     :: [KDE]
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

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
