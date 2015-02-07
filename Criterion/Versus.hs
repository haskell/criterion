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
       ) where

import Criterion.Types
import Criterion.Monad (Criterion)
import Control.Monad.Trans (liftIO)
import Control.Monad
import Data.Csv as Csv
import qualified Data.Map as M
import Data.List (isPrefixOf, stripPrefix, find)
import Statistics.Resampling.Bootstrap (Estimate)

data VersusReport = VersusReport String [((String, String), Estimate)]

vscsv :: [Report]
      -> Criterion ()
vscsv r = do
  -- liftIO $ print b
  -- liftIO $ forM_ r $ \i -> putStr (reportName i) >> putStr " -> " >> print (anMean $ reportAnalysis i)
  return ()

