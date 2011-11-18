{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

-- |
-- Module      : Criterion.Report
-- Copyright   : (c) 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reporting functions.

module Criterion.Report
    (
      Report(..)
    , report
    ) where

import Control.Monad.IO.Class (liftIO)
import Criterion.Analysis (Outliers(..), SampleAnalysis(..))
import Criterion.Monad (Criterion)
import Data.ByteString.Char8 ()
import Data.Char (isSpace, toLower)
import Data.Data (Data, Typeable)
import Data.List (group)
import Paths_criterion (getDataFileName)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample)
import System.FilePath (isPathSeparator, joinPath)
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkGenericContext, mkStrContext)
import Text.Printf (printf)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Text.Hastache as H

data Report = Report {
      reportNumber :: Int
    , reportName :: String
    , reportTimes :: Sample
    , reportAnalysis :: SampleAnalysis
    , reportOutliers :: Outliers
    } deriving (Eq, Show, Typeable, Data)

templateDir :: FilePath
templateDir = "templates"

templatePath :: FilePath
templatePath = joinPath [templateDir,"report.tpl"]

javascriptPath :: FilePath
javascriptPath = joinPath [templateDir,"js"]

report :: String -> [Report] -> Criterion ()
report name reports = do
  jsURI <- fmap pathToURI . liftIO $ getDataFileName javascriptPath
  tplURI <- fmap pathToURI . liftIO $ getDataFileName templateDir
  let context "report"  = MuList $ map inner reports
      context "jspath"  = MuVariable jsURI
      context "tplpath" = MuVariable tplURI
      context _         = MuNothing
      inner Report{..} = mkStrContext $ \nym ->
                         case nym of
                           "name"     -> MuVariable reportName
                           "number"   -> MuVariable reportNumber
                           "times"    -> enc reportTimes
                           "kdetimes" -> enc kdeTimes
                           "kdepdf"   -> enc kdePDF
                           ('a':'n':_)-> mkGenericContext reportAnalysis $
                                         H.encodeStr nym
                           _          -> mkGenericContext reportOutliers $
                                         H.encodeStr nym
          where (kdeTimes,kdePDF) = kde 128 reportTimes
      enc :: (A.ToJSON a) => a -> MuType m
      enc = MuVariable . A.encode
  tplPath <- liftIO $ getDataFileName templatePath
  bs <- liftIO $ H.hastacheFile H.defaultConfig tplPath context
  liftIO $ L.writeFile (safePath $ printf "%s report.html" name) bs
  return ()

pathToURI :: FilePath -> String
pathToURI = map (replace isPathSeparator '/')

-- | Get rid of spaces and other potentially troublesome characters
-- from a file name.
safePath :: String -> FilePath
safePath = concatMap (replace ((==) '-' . head) "-")
       . group
       . map (replace isSpace '-' . replace isPathSeparator '-' . toLower)

replace :: (a -> Bool) -> a -> a -> a
replace p r c | p c       = r
              | otherwise = c
