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
         vscsv
       , versusReport
       ) where

import Criterion.Types
import Criterion.Monad (Criterion)
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Csv as Csv
import qualified Data.Map as M
import Data.List (groupBy)
import Statistics.Resampling.Bootstrap (Estimate)

data VersusReport = VersusReport {
      vsReportDescription :: String
    , vsReportData :: [(String, [(String, Estimate)])]
    } deriving (Eq, Show, Read)

isVersus :: Report -> Bool
isVersus r = case reportOwner r of
              (_ : ROVersus _ _ _ : _) -> True
              _                        -> False

vs :: Report -> ReportOwner
vs = tail . reportOwner

alg :: Report -> String
alg = roTypeVsAlg . head . vs

benv :: Report -> String
benv = roTypeVsEnv . head . vs

eqVs :: ReportOwner -> ReportOwner -> Bool
eqVs (ROVersus d1 _ _:t1) (ROVersus d2 _ _ :t2) = d1 == d2 && t1 == t2
eqVs _ _ = error "eqVs: shouldn't happen"

versusName :: ReportOwner -> String
versusName (v:t) = reportOwnerToName $ ROBench (roTypeVsTag v) : t

groupAlgs :: [Report] -> [(String, [(String, Estimate)])]
groupAlgs r = map f r'
  where r' = groupBy ((==)`on`alg) r
        f = alg . head &&& map (benv &&& anMean . reportAnalysis)

groupBGroups :: [Report] -> [(String, [Report])]
groupBGroups r = map ((versusName . vs . head) &&& id) r'
  where r' = groupBy (eqVs `on` vs) $ filter isVersus r

versusReport :: [Report] -> [VersusReport]
versusReport r = map (\(t, l) -> VersusReport t $ groupAlgs l) rr
  where rr  = groupBGroups r
