{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings,
    RecordWildCards, ScopedTypeVariables #-}

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
    , formatReport
    , report
    -- * Rendering helper functions
    , TemplateException(..)
    , loadTemplate
    , includeFile
    , templateDir
    , vector
    , vector2
    ) where

import Control.Exception (Exception, IOException, throwIO)
import Control.Monad (mplus)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Criterion.Analysis (Outliers(..), SampleAnalysis(..))
import Criterion.Config (cfgReport, cfgTemplate, fromLJ)
import Criterion.Monad (Criterion, getConfig)
import Data.Data (Data, Typeable)
import Data.Monoid (Last(..))
import GHC.Generics (Generic)
import Paths_criterion (getDataFileName)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample)
import System.Directory (doesFileExist)
import System.FilePath ((</>), isPathSeparator)
import System.IO.Unsafe (unsafePerformIO)
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkGenericContext, mkStrContext, mkStrContextM)
import qualified Control.Exception as E
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
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | The path to the template and other files used for generating
-- reports.
templateDir :: FilePath
templateDir = unsafePerformIO $ getDataFileName "templates"
{-# NOINLINE templateDir #-}

-- | Write out a series of 'Report' values to a single file, if
-- configured to do so.
report :: [Report] -> Criterion ()
report reports = do
  cfg <- getConfig
  case cfgReport cfg of
    Last Nothing -> return ()
    Last (Just name) -> liftIO $ do
      tpl <- loadTemplate [".",templateDir] (fromLJ cfgTemplate cfg)
      L.writeFile name =<< formatReport reports tpl

-- | Format a series of 'Report' values using the given Hastache
-- template.
formatReport :: [Report]
             -> B.ByteString    -- ^ Hastache template.
             -> IO L.ByteString
formatReport reports template = do
  let context "report"  = return $ MuList $ map inner reports
      context "include" = return $ MuLambdaM $ includeFile [templateDir]
      context _         = return $ MuNothing
      inner Report{..} = mkStrContextM $ \nym ->
                         case nym of
                           "name"     -> return $ MuVariable reportName
                           "number"   -> return $ MuVariable reportNumber
                           "times"    -> return $ vector "x" reportTimes
                           "kdetimes" -> return $ vector "x" kdeTimes
                           "kdepdf"   -> return $ vector "x" kdePDF
                           "kde"      -> return $ vector2 "time" "pdf" kdeTimes kdePDF
                           ('a':'n':_)-> mkGenericContext reportAnalysis $
                                         H.encodeStr nym
                           _          -> mkGenericContext reportOutliers $
                                         H.encodeStr nym
          where (kdeTimes,kdePDF) = kde 128 reportTimes
  H.hastacheStr H.defaultConfig template context

-- | Render the elements of a vector.
--
-- For example, given this piece of Haskell:
--
-- @'mkStrContext' $ \\name ->
-- case name of
--   \"foo\" -> 'vector' \"x\" foo@
--
-- It will substitute each value in the vector for @x@ in the
-- following Hastache template:
--
-- > {{#foo}}
-- >  {{x}}
-- > {{/foo}}
vector :: (Monad m, G.Vector v a, H.MuVar a) =>
          String                -- ^ Name to use when substituting.
       -> v a
       -> MuType m
{-# SPECIALIZE vector :: String -> U.Vector Double -> MuType IO #-}
vector name v = MuList . map val . G.toList $ v
    where val i = mkStrContext $ \nym ->
                  if nym == name
                  then MuVariable i
                  else MuNothing

-- | Render the elements of two vectors.
vector2 :: (Monad m, G.Vector v a, G.Vector v b, H.MuVar a, H.MuVar b) =>
           String               -- ^ Name for elements from the first vector.
        -> String               -- ^ Name for elements from the second vector.
        -> v a                  -- ^ First vector.
        -> v b                  -- ^ Second vector.
        -> MuType m
{-# SPECIALIZE vector2 :: String -> String -> U.Vector Double -> U.Vector Double
                       -> MuType IO #-}
vector2 name1 name2 v1 v2 = MuList $ zipWith val (G.toList v1) (G.toList v2)
    where val i j = mkStrContext $ \nym ->
                    case undefined of
                      _| nym == name1 -> MuVariable i
                       | nym == name2 -> MuVariable j
                       | otherwise    -> MuNothing

-- | Attempt to include the contents of a file based on a search path.
-- Returns 'B.empty' if the search fails or the file could not be read.
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
            B.readFile path `E.catch` \(_::IOException) -> next

-- | A problem arose with a template.
data TemplateException =
    TemplateNotFound FilePath   -- ^ The template could not be found.
    deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Exception TemplateException

-- | Load a Hastache template file.
--
-- If the name is an absolute or relative path, the search path is
-- /not/ used, and the name is treated as a literal path.
--
-- This function throws a 'TemplateException' if the template could
-- not be found, or an 'IOException' if no template could be loaded.
loadTemplate :: [FilePath]      -- ^ Search path.
             -> FilePath        -- ^ Name of template file.
             -> IO B.ByteString
loadTemplate paths name
    | any isPathSeparator name = B.readFile name
    | otherwise                = go Nothing paths
  where go me (p:ps) = do
          let cur = p </> name
          x <- doesFileExist cur
          if x
            then B.readFile cur `E.catch` \e -> go (me `mplus` Just e) ps
            else go me ps
        go (Just e) _ = throwIO (e::IOException)
        go _        _ = throwIO . TemplateNotFound $ name
