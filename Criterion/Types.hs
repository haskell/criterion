{-# LANGUAGE FlexibleInstances, GADTs #-}
module Criterion.Types
    (
      Benchmarkable(..)
    , Benchmark(..)
    , bench
    , bgroup
    , benchNames
    ) where

import Control.Parallel.Strategies
import Control.Exception (evaluate)

class Benchmarkable b where
    run :: b -> Int -> IO ()

instance (NFData a) => Benchmarkable (Int -> a) where
    run f u = evaluate (f u) >> return ()

instance Benchmarkable (IO a) where
    run a _ = a >> return ()

data Benchmark where
    Benchmark  :: Benchmarkable b => String -> b -> Benchmark
    BenchGroup :: String -> [Benchmark] -> Benchmark

bench :: Benchmarkable b => String -> b -> Benchmark
bench = Benchmark

bgroup :: String -> [Benchmark] -> Benchmark
bgroup = BenchGroup

benchNames :: Benchmark -> [String]
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = d : concatMap benchNames bs

instance Show Benchmark where
    show (Benchmark d _)  = ("Benchmark " ++ show d)
    show (BenchGroup d _) = ("BenchGroup " ++ show d)
