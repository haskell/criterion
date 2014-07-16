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
    , Result(..)
    , Payload(..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Criterion.Analysis.Types (Outliers(..), SampleAnalysis(..))
import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | A pure function or impure action that can be benchmarked. The
-- 'Int' parameter indicates the number of times to run the given
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
        d k = maybe k (/ iters) (double k)
        i k = maybe k (round . (/ iters)) (fromIntegral <$> int k)
        iters               = fromIntegral measIters :: Double

int :: Int64 -> Maybe Int64
int i | i == minBound = Nothing
      | otherwise     = Just i

double :: Double -> Maybe Double
double d | isInfinite d || isNaN d = Nothing
         | otherwise               = Just d


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

data Payload = Payload {
      sample         :: V.Vector Measured
    , sampleAnalysis :: SampleAnalysis
    , outliers       :: Outliers
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

measure :: (U.Unbox a) => (Measured -> a) -> V.Vector Measured -> U.Vector a
measure f v = U.convert . V.map f $ v

instance Binary Payload where
    put (Payload x y z) = put x >> put y >> put z
    get = Payload <$> get <*> get <*> get

data Result = Single String Payload
              deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Result where
    put (Single x y) = put x >> put y
    get = Single <$> get <*> get
