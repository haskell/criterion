{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Criterion
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core benchmarking code.

module Criterion
    (
      Benchmarkable(..)
    , Benchmark
    , nf
    , whnf
    , nfIO
    , whnfIO
    , bench
    , bgroup
    , runBenchmark
    , runAndAnalyse
    , runNotAnalyse
    ) where

import Criterion.Internal
import Criterion.Types
