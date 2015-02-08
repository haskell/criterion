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
import Criterion.IO.Printf (writeCsv)
import Criterion.Monad (Criterion)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Csv as Csv
import qualified Data.Map as M
import Data.List (groupBy)
import Statistics.Resampling.Bootstrap (Estimate(..))

data VersusReport = VersusReport {
      vsReportDescription :: String
    , vsReportDataPoints :: [String]
    , vsReportData :: [(String, [Estimate])]
    } deriving (Eq, Show, Read)

{-
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

groupBy' :: (a -> a -> Bool) -> (a -> b) -> [a] -> [(b, [a])]
groupBy' f g = map (g . head &&& id) . groupBy f

versusReport :: [Report] -> [VersusReport]
versusReport = map mkVsReport . groupBy' (eqVs `on` vs) vs . filter isVersus
  where mkVsReport (ro, rpts) = VersusReport {
           vsReportDescription = reportOwnerToName ro
         , vsReportDataPoints  = map benv . snd $ head ralg
         , vsReportData        = []
         }
         where ralg = groupBy' ((==) `on` alg) alg rpts

vscsv :: [VersusReport] -> Criterion ()
vscsv rpts = do
  file <- asks vsCsvFile
  forM_ rpts $ \r -> do
    writeCsv file [vsReportDescription r]
    writeCsv file $ "Name" : vsReportDataPoints r
    --forM_ d $ \(a, f) -> writeCsv file $ a : map (show.estPoint.snd) f
-}

vscsv = undefined

versusReport = undefined
