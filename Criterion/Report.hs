{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards, ViewPatterns #-}

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

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Criterion.Analysis (Outliers(..), SampleAnalysis(..))
import Criterion.IO (note)
import Criterion.Monad (Criterion)
import Data.Char (isSpace, toLower)
import Data.Data (Data, Typeable)
import Data.List (group)
import Paths_criterion (getDataFileName)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample)
import System.Environment (getProgName)
import System.FilePath ((</>), takeFileName)
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkGenericContext, mkStrContext)
import Text.Printf (printf)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
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

report :: [Report] -> Criterion ()
report reports = do
  tpl <- liftIO $ getDataFileName templateDir
  let context "report"  = MuList $ map inner reports
      context "include" = MuLambdaM $ \n ->
                            liftIO $ B.readFile (tpl </> H.decodeStr n)
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
  rep <- liftIO $ do
    bs <- H.hastacheFile H.defaultConfig (tpl </> "report.tpl") context
    progName <- takeFileName <$> getProgName
    let name = safePath $ printf "%s criterion.html" progName
    L.writeFile name bs
    return name
  note "report written to %s\n" rep

-- | Get rid of spaces and other potentially troublesome characters
-- from a file name.
safePath :: String -> FilePath
safePath = concatMap (replace ((==) '-' . head) "-")
       . group
       . map (replace isSpace '-' . replace (`elem` "\"'();/\\") '-' . toLower)

replace :: (a -> Bool) -> a -> a -> a
replace p r c | p c       = r
              | otherwise = c
