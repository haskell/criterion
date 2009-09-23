{-# LANGUAGE FlexibleInstances, GADTs #-}
module Criterion.Types
    (
      Benchmarkable(..)
    , Benchmark(..)
    , bench
    , benchName
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
    Benchmark :: Benchmarkable b => String -> b -> Benchmark

bench :: Benchmarkable b => String -> b -> Benchmark
bench = Benchmark

benchName :: Benchmark -> String
benchName (Benchmark d _) = d

instance Show Benchmark where
    show = ("Benchmark " ++) . benchName
