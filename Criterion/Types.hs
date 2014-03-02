{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ExistentialQuantification,
    FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- |
-- Module      : Criterion.Types
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
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
import Data.Binary (Binary (..))
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Statistics.Types (Sample)

-- | A pure function or impure action that can be benchmarked. The
-- 'Int' parameter indicates the number of times to run the given
-- function or action.
newtype Benchmarkable = Benchmarkable (Int -> IO ())

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable
whnf = pure id
{-# INLINE whnf #-}

-- | Apply an argument to a function, and evaluate the result to head
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = pure rnf
{-# INLINE nf #-}

pure :: (b -> c) -> (a -> b) -> a -> Benchmarkable
pure reduce f0 x0 = Benchmarkable $ go f0 x0
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)
{-# INLINE pure #-}

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
      sample         :: Sample
    , sampleAnalysis :: SampleAnalysis
    , outliers       :: Outliers
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Payload where
    put (Payload x y z) = put x >> put y >> put z
    get = Payload <$> get <*> get <*> get

data Result = Single String Payload
              deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Binary Result where
    put (Single x y) = put x >> put y
    get = Single <$> get <*> get
