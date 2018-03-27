{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Criterion.Measurement
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmark measurement code.

module Criterion.Measurement
    (
      initializeTime
    , getTime
    , getCPUTime
    , getCycles
    , getGCStatistics
    , GCStatistics(..)
    , secs
    , measure
    , runBenchmark
    , runBenchmarkable
    , runBenchmarkable_
    , measured
    , applyGCStatistics
    , threshold
    )
    where

import "criterion-measurement" Criterion.Measurement

