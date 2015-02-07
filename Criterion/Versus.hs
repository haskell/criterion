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
         versusReports
       , vscsv
       ) where

import Criterion.Types
import Criterion.Monad (Criterion)
import Control.Monad.Trans (liftIO)
import Control.Monad
import Data.Csv as Csv
import qualified Data.Map as M
import Data.List (isPrefixOf, stripPrefix, find)
import Statistics.Resampling.Bootstrap (Estimate)

getVs :: String
      -> Benchmark
      -> [Benchmark]
getVs p (Environment _ b)     = getVs p $ b undefined
getVs _ (Benchmark _ _)       = []
getVs p (BenchGroup d b)      = b >>= getVs (p++d++"/")
getVs p (BenchVersus d e a)   = [BenchVersus (p++d) e a]

data VersusReport = VersusReport [String, ]

versusReport :: Benchmark
          -> [Report]
          -> Maybe VersusReport
versusReport bnch reps = do
  let vss = getVs "" bnch
      vss' = zip vss $ replicate M.empty
      f m Report {reportName = n, reportAnalysis = a} =
        case find (\i -> isPrefixOf a n) vss of
         Just s  -> M.update () s m 
         Nothing -> m
         where 
      vs  = foldl f vss' reps
  
  
vscsv :: Benchmark
      -> [Report]
      -> Criterion ()
vscsv b r = do
  liftIO $ print b
  liftIO $ forM_ r $ \i -> putStr (reportName i) >> putStr " -> " >> print (anMean $ reportAnalysis i)

