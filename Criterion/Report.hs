{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables #-}

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
    -- * Rendering helper functions
    , includeFile
    , templateDir
    , vector
    , vector2
    ) where

import Control.Applicative ((<$>))
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Criterion.Analysis (Outliers(..), SampleAnalysis(..))
import Criterion.IO (note)
import Criterion.Monad (Criterion)
import Data.Char (isSpace, toLower)
import Data.Data (Data, Typeable)
import Data.List (group)
import Paths_criterion (getDataFileName)
import Prelude hiding (catch)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample)
import System.Environment (getProgName)
import System.FilePath ((</>), takeFileName)
import System.IO.Unsafe (unsafePerformIO)
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkGenericContext, mkStrContext)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Text.Hastache as H

data Report = Report {
      reportNumber :: Int
    , reportName :: String
    , reportTimes :: Sample
    , reportAnalysis :: SampleAnalysis
    , reportOutliers :: Outliers
    } deriving (Eq, Show, Typeable, Data)

-- | The path to the template and other files used for generating
-- reports.
templateDir :: FilePath
templateDir = unsafePerformIO $ getDataFileName "templates"
{-# NOINLINE templateDir #-}

-- | Write out a series of 'Report' values to a single file.
report :: [Report] -> Criterion ()
report reports = do
  let context "report"  = MuList $ map inner reports
      context "include" = MuLambdaM $ includeFile [templateDir]
      context _         = MuNothing
      inner Report{..} = mkStrContext $ \nym ->
                         case nym of
                           "name"     -> MuVariable reportName
                           "number"   -> MuVariable reportNumber
                           "times"    -> vector "x" reportTimes
                           "kdetimes" -> vector "x" kdeTimes
                           "kdepdf"   -> vector "x" kdePDF
                           "kde"      -> vector2 "time" "pdf" kdeTimes kdePDF
                           ('a':'n':_)-> mkGenericContext reportAnalysis $
                                         H.encodeStr nym
                           _          -> mkGenericContext reportOutliers $
                                         H.encodeStr nym
          where (kdeTimes,kdePDF) = kde 128 reportTimes
  rep <- liftIO $ do
    bs <- H.hastacheFile H.defaultConfig (templateDir </> "report.tpl") context
    progName <- takeFileName <$> getProgName
    let name = safePath $ printf "%s criterion.html" progName
    L.writeFile name bs
    return name
  note "report written to %s\n" rep

vector :: (Monad m, G.Vector v a, H.MuVar a) =>
          String -> v a -> MuType m
{-# SPECIALIZE vector :: String -> U.Vector Double -> MuType IO #-}
vector name v = MuList . map val . G.toList $ v
    where val i = mkStrContext $ \nym ->
                  if nym == name
                  then MuVariable i
                  else MuNothing

vector2 :: (Monad m, G.Vector v a, G.Vector v b, H.MuVar a, H.MuVar b) =>
           String -> String -> v a -> v b -> MuType m
{-# SPECIALIZE vector2 :: String -> String -> U.Vector Double -> U.Vector Double
                       -> MuType IO #-}
vector2 name1 name2 v1 v2 = MuList $ zipWith val (G.toList v1) (G.toList v2)
    where val i j = mkStrContext $ \nym ->
                    case undefined of
                      _| nym == name1 -> MuVariable i
                       | nym == name2 -> MuVariable j
                       | otherwise    -> MuNothing

-- | Get rid of spaces and other potentially troublesome characters
-- from a file name.
safePath :: String -> FilePath
safePath = concatMap (replace ((==) '-' . head) "-")
       . group
       . map (replace isSpace '-' . replace (`elem` "\"'();/\\") '-' . toLower)

replace :: (a -> Bool) -> a -> a -> a
replace p r c | p c       = r
              | otherwise = c

-- | Attempt to include the contents of a file based on a search path.
-- Returns 'B.empty' if the search fails.
--
-- Intended for use with Hastache's 'MuLambdaM', for example:
--
-- @context \"include\" = 'MuLambdaM' $ 'includeFile' ['templateDir']@
--
-- Hastache template expansion is /not/ performed within the included
-- file.  No attempt is made to ensure that the included file path is
-- safe, i.e. that it does not refer to an unexpected file such as
-- \"@/etc/passwd@\".
includeFile :: (MonadIO m) =>
               [FilePath]       -- ^ Directories to search.
            -> B.ByteString     -- ^ Name of the file to search for.
            -> m B.ByteString
{-# SPECIALIZE includeFile :: [FilePath] -> B.ByteString -> IO B.ByteString #-}
includeFile searchPath name = liftIO $ foldr go (return B.empty) searchPath
    where go dir next = do
            let path = dir </> H.decodeStr name
            B.readFile path `catch` \(_::IOException) -> next
