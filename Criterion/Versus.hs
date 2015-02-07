{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Criterion
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- 'Versus'-type bgroup support

module Criterion.Versus
       (
         getVsNames
       , vscsv
       ) where

import Criterion.Types
import Criterion.Monad (Criterion)
import Control.Monad.Trans (liftIO)
import Control.Monad
import Data.Csv as Csv

getVsNames :: Benchmark -> [String]
getVsNames (Environment _ b)   = getVsNames $ b undefined
getVsNames (Benchmark _ _)     = []
getVsNames (BenchGroup d b)    = map ((d ++ "/") ++) $ b >>= getVsNames
getVsNames (BenchVersus d _ _) = [d]

vscsv :: Benchmark
      -> [Report]
      -> Criterion ()
vscsv b r = do
  liftIO $ print b
  liftIO $ forM_ r $ \i -> putStr (reportName i) >> putStr " -> " >> print (anMean $ reportAnalysis i)
