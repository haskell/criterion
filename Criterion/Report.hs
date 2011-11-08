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
import Criterion.Monad (Criterion)
import Data.Char (isSpace, toLower)
import Data.Data (Data, Typeable)
import Data.List (group)
import Paths_criterion (getDataFileName)
import qualified Data.Aeson as A
import Statistics.Types (Sample)
import System.FilePath (isPathSeparator)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 ()
import qualified Data.Vector.Unboxed as U
import qualified Text.Hastache as H
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkStrContext)
import Statistics.Sample (mean)

data Report = Report {
      reportName :: String
    , reportNumber :: Int
    , reportTimes :: Sample
    , reportKDE :: (U.Vector Double, U.Vector Double)
    } deriving (Eq, Show, Typeable, Data)

templatePath :: FilePath
templatePath = "templates/report.tpl"

report :: String -> [Report] -> Criterion ()
report name reports = do
  let context "report" = MuList $ map inner reports
      context _        = MuNothing
      inner Report{..} = mkStrContext $ \ctx ->
                         case ctx of
                           "name"   -> MuVariable (H.encodeStrLBS reportName)
                           "number" -> MuVariable reportNumber
                           "times"  -> enc (U.map (*scale) reportTimes)
                           "units"  -> MuVariable units
                           "kde"    -> enc (U.zip (U.map (*scale) kdeTimes)
                                                  (U.map (/U.maximum kdePDF) kdePDF))
                           _        -> MuNothing
          where (scale,units)     = unitsOf (mean reportTimes)
                (kdeTimes,kdePDF) = reportKDE
      enc :: (A.ToJSON a) => a -> MuType m
      enc = MuVariable . A.encode
  tplPath <- liftIO $ getDataFileName templatePath
  bs <- liftIO $ H.hastacheFile H.defaultConfig tplPath context
  liftIO $ L.writeFile (safePath $ printf "%s report.html" name) bs
  return ()

-- | Get rid of spaces and other potentially troublesome characters
-- from a file name.
safePath :: String -> FilePath
safePath = concatMap (replace ((==) '-' . head) "-")
       . group
       . map (replace isSpace '-' . replace isPathSeparator '-' . toLower)
    where replace p r c | p c       = r
                        | otherwise = c

unitsOf :: Double -> (Double,String)
unitsOf k
  | k < 0      = unitsOf (-k)
  | k >= 1e9   = (1e-9, "Gs")
  | k >= 1e6   = (1e-6, "Ms")
  | k >= 1e4   = (1e-3, "Ks")
  | k >= 1     = (1,    "s")
  | k >= 1e-3  = (1e3,  "ms")
  | k >= 1e-6  = (1e6,  "\956s")
  | k >= 1e-9  = (1e9,  "ns")
  | k >= 1e-12 = (1e12, "ps")
  | otherwise  = (1,    "s")
