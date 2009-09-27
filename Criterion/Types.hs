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
-- argument, and reduces the result the function returns to WHNF.

{-# LANGUAGE FlexibleInstances, GADTs #-}
module Criterion.Types
    (
      Benchmarkable(..)
    , Benchmark(..)
    , bench
    , bgroup
    , benchNames
    ) where

import Control.Exception (evaluate)

-- | A benchmarkable function or action.
class Benchmarkable b where
    run :: b -> Int -> IO ()

instance Benchmarkable (Int -> a) where
    run f u = evaluate (f u) >> return ()

instance Benchmarkable (IO a) where
    run a _ = a >> return ()

-- | A benchmark may be composed of either a single 'Benchmarkable'
-- item with a name, or a (possibly nested) group of 'Benchmark's.
data Benchmark where
    Benchmark  :: Benchmarkable b => String -> b -> Benchmark
    BenchGroup :: String -> [Benchmark] -> Benchmark

-- | Create a single benchmark.
bench :: Benchmarkable b => String -> b -> Benchmark
bench = Benchmark

-- | Group several benchmarks together under a common name.
bgroup :: String -> [Benchmark] -> Benchmark
bgroup = BenchGroup

-- | Retrieve the names of all benchmarks.
benchNames :: Benchmark -> [String]
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = map ((d ++ "/") ++) . concatMap benchNames $ bs

instance Show Benchmark where
    show (Benchmark d _)  = ("Benchmark " ++ show d)
    show (BenchGroup d _) = ("BenchGroup " ++ show d)
