{-# LANGUAGE BangPatterns, GADTs, StandaloneDeriving, OverloadedStrings #-}
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
       , versusReports
       , VersusReport(..)
       ) where

import Criterion.Types
import Criterion.IO.Printf (writeCsv)
import Criterion.Monad (Criterion)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import Control.Arrow ((&&&))
import Data.Function (on)
import qualified Data.Csv as Csv
import Data.List (groupBy, sortBy)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Data.Aeson (ToJSON(..), object, encode, (.=))

data VersusReport where
  VersusReport :: (Show l, Ord l) => {
    vsReportDescription :: String
  , vsReportDataPoints :: [l]
  , vsReportData :: [(String, [Estimate])]
  , vsReportIndices :: [((String, l), Int)]
  } -> VersusReport
deriving instance Show VersusReport

{- This instance is actually incomplete -}
instance ToJSON VersusReport where
  toJSON VersusReport{
      vsReportDescription = desc
    , vsReportDataPoints  = dp
    , vsReportData        = d
    , vsReportIndices     = i
    } = object [ "name"       .= toJSON desc
               , "dataPoints" .= toJSON (map show dp)
               , "data"       .= toJSON (map mkArr d)]
        where
          mkArr (alg, p) = object [ "alg"  .= toJSON alg
                                  , "data" .= toJSON p]

vscsv :: [VersusReport] -> Criterion ()
vscsv = mapM_ f
  where f VersusReport{
            vsReportDescription = d
          , vsReportData        = r
          , vsReportDataPoints  = p} = do
          file <- asks vsCsvFile
          writeCsv file [d]
          writeCsv file $ "name":(map show p)
          forM_ r $ \(a, m) -> writeCsv file $ a:(map (show . estPoint) m)


versusReports :: [VersusReport] -> [Report] -> [VersusReport]
versusReports vrpts rpts = map (vsReport rpts) vrpts

vsReport :: [Report] -> VersusReport -> VersusReport
vsReport rpts vr@VersusReport{vsReportIndices = indices} =
  vr{vsReportData = map f l}
  where
    alg = fst . fst
    env = snd . fst
    l   = groupBy' ((==) `on` alg) alg indices
    rpts' = sortBy (compare `on` reportNumber) rpts
    f (alg, idx) = (alg, [anMean . reportAnalysis $ rpts'!!i
                         | (_, i)<-sortBy (compare `on` env) idx])

groupBy' :: (a -> a -> Bool) -> (a -> b) -> [a] -> [(b, [a])]
groupBy' f g = map (g . head &&& id) . groupBy f
