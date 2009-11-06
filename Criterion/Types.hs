-- |
-- Module      : Criterion.Types
-- Copyright   : (c) Bryan O'Sullivan 2009
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
-- For a pure function of type @Int -> a@, the benchmarking harness
-- calls this function repeatedly, each time with a different 'Int'
-- argument, and reduces the result the function returns to weak head
-- normal form.  If you need the result reduced to normal form, that
-- is your responsibility.
--
-- For an action of type @IO a@, the benchmarking harness calls the
-- action repeatedly, but does not reduce the result.

{-# LANGUAGE FlexibleInstances, GADTs #-}
module Criterion.Types
    (
      Benchmarkable(..)
    , Benchmark(..)
    , B(..)
    , bench
    , bgroup
    , benchNames
    ) where

import Control.Exception (evaluate)

-- | A benchmarkable function or action.
class Benchmarkable a where
    -- | Run a function or action the specified number of times.
    run :: a                    -- ^ The function or action to benchmark.
        -> Int                  -- ^ The number of times to run or evaluate it.
        -> IO ()

-- | A container for a pure function to benchmark, and an argument to
-- supply to it each time it is evaluated.
data B a b = B (a -> b) a

instance Benchmarkable (a -> b, a) where
    run fx@(f,x) n
        | n <= 0    = return ()
        | otherwise = evaluate (f x) >> run fx (n-1)
    {-# INLINE run #-}

instance Benchmarkable (B a b) where
    run fx@(B f x) n
        | n <= 0    = return ()
        | otherwise = evaluate (f x) >> run fx (n-1)
    {-# INLINE run #-}

instance Benchmarkable (IO a) where
    run a n
        | n <= 0    = return ()
        | otherwise = a >> run a (n-1)
    {-# INLINE run #-}

-- | A benchmark may consist of either a single 'Benchmarkable' item
-- with a name, created with 'bench', or a (possibly nested) group of
-- 'Benchmark's, created with 'bgroup'.
data Benchmark where
    Benchmark  :: Benchmarkable b => String -> b -> Benchmark
    BenchGroup :: String -> [Benchmark] -> Benchmark

-- | Create a single benchmark.
bench :: Benchmarkable b =>
         String                 -- ^ A name to identify the benchmark.
      -> b
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
