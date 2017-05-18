{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings,
    RecordWildCards, ScopedTypeVariables #-}

-- |
-- Module      : Criterion.Report
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reporting functions.

module Criterion.Report
    (
      formatReport
    , report
    , tidyTails
    -- * Rendering helper functions
    , TemplateException(..)
    , loadTemplate
    , includeFile
    , getTemplateDir
    , vector
    , vector2
    ) where

import Control.Exception (Exception, IOException, throwIO)
import Control.Monad (mplus)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ask)
import Criterion.Monad (Criterion)
import Criterion.Types
import Data.Aeson (ToJSON (..), Value(..), object, (.=), Value, encode)
import Data.Data (Data, Typeable)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Paths_criterion (getDataFileName)
import Statistics.Function (minMax)
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>), isPathSeparator)
import Text.Microstache (Key (..), Template (..), Node (..), compileMustacheText, renderMustache)
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Language.Javascript.Flot as Flot
import qualified Language.Javascript.JQuery as JQuery

-- | Trim long flat tails from a KDE plot.
tidyTails :: KDE -> KDE
tidyTails KDE{..} = KDE { kdeType   = kdeType
                        , kdeValues = G.slice front winSize kdeValues
                        , kdePDF    = G.slice front winSize kdePDF
                        }
  where tiny     = uncurry subtract (minMax kdePDF) * 0.005
        omitTiny = G.length . G.takeWhile ((<= tiny) . abs)
        front    = omitTiny kdePDF
        back     = omitTiny . G.reverse $ kdePDF
        winSize  = G.length kdePDF - front - back

-- | Return the path to the template and other files used for
-- generating reports.
getTemplateDir :: IO FilePath
getTemplateDir = getDataFileName "templates"

-- | Write out a series of 'Report' values to a single file, if
-- configured to do so.
report :: [Report] -> Criterion ()
report reports = do
  Config{..} <- ask
  forM_ reportFile $ \name -> liftIO $ do
    td <- getTemplateDir
    tpl <- loadTemplate [td,"."] template
    TL.writeFile name =<< formatReport reports tpl

-- | Format a series of 'Report' values using the given Mustache template.
formatReport :: [Report]
             -> TL.Text    -- ^ Mustache template.
             -> IO TL.Text
formatReport reports templateName = do
    template0 <- case compileMustacheText "tpl" templateName of
        Left err -> fail (show err) -- TODO: throw a template exception?
        Right x -> return x

    jQuery <- T.readFile =<< JQuery.file
    flot <- T.readFile =<< Flot.file Flot.Flot

    -- includes, only top level
    templates <- getTemplateDir
    template <- includeTemplate (includeFile [templates]) template0

    let context = object
            [ "json"      .= reports
            , "report"    .= map inner reports
            , "js-jquery" .= jQuery
            , "js-flot"   .= flot
            ]

    return (renderMustache template context)
  where
    includeTemplate :: (FilePath -> IO T.Text) -> Template -> IO Template
    includeTemplate f Template {..} = fmap
        (Template templateActual)
        (mapM (mapM (includeNode f)) templateCache)

    includeNode :: (FilePath -> IO T.Text) -> Node -> IO Node
    includeNode f (Section (Key ["include"]) [TextBlock fp]) =
        fmap TextBlock (f (T.unpack fp))
    includeNode _ n = return n

    -- Merge Report with it's analysis and outliers
    merge :: ToJSON a => a -> Value -> Value
    merge x y = case toJSON x of
        Object x' -> case y of
            Object y' -> Object (x' <> y')
            _         -> y
        _         -> y

    inner r@Report {..} = merge reportAnalysis $ merge reportOutliers $ object
        [ "name"     .= reportName
        , "json"     .= TLE.decodeUtf8 (encode r)
        , "number"   .= reportNumber
        , "iters"    .= vector "x" iters
        , "times"    .= vector "x" times
        , "cycles"   .= vector "x" cycles
        , "kdetimes" .= vector "x" kdeValues
        , "kdepdf"   .= vector "x" kdePDF
        , "kde"      .= vector2 "time" "pdf" kdeValues kdePDF
        ]
      where
        [KDE{..}]   = reportKDEs
        iters       = measure measIters reportMeasured
        times       = measure measTime reportMeasured
        cycles      = measure measCycles reportMeasured
{-
                           ('a':'n':_)-> mkGenericContext reportAnalysis $
                                         H.encodeStr nym
                           _          -> mkGenericContext reportOutliers $
                                         H.encodeStr nym
-}

-- | Render the elements of a vector.
--
-- It will substitute each value in the vector for @x@ in the
-- following Mustache template:
--
-- > {{#foo}}
-- >  {{x}}
-- > {{/foo}}
vector :: (G.Vector v a, ToJSON a) =>
          T.Text                -- ^ Name to use when substituting.
       -> v a
       -> Value
{-# SPECIALIZE vector :: T.Text -> U.Vector Double -> Value #-}
vector name v = toJSON . map val . G.toList $ v where
    val i = object [ name .= i ]

-- | Render the elements of two vectors.
vector2 :: (G.Vector v a, G.Vector v b, ToJSON a, ToJSON b) =>
           T.Text               -- ^ Name for elements from the first vector.
        -> T.Text               -- ^ Name for elements from the second vector.
        -> v a                  -- ^ First vector.
        -> v b                  -- ^ Second vector.
        -> Value
{-# SPECIALIZE vector2 :: T.Text -> T.Text -> U.Vector Double -> U.Vector Double
                       -> Value #-}
vector2 name1 name2 v1 v2 = toJSON $ zipWith val (G.toList v1) (G.toList v2) where
    val i j = object
        [ name1 .= i
        , name2 .= j
        ]

-- | Attempt to include the contents of a file based on a search path.
-- Returns 'B.empty' if the search fails or the file could not be read.
--
-- Intended for preprocessing Mustache files, e.g. replacing sections
--
-- @
-- {{#include}}file.txt{{/include}
-- @
--
-- with file contents.
includeFile :: (MonadIO m) =>
               [FilePath]       -- ^ Directories to search.
            -> FilePath         -- ^ Name of the file to search for.
            -> m T.Text
{-# SPECIALIZE includeFile :: [FilePath] -> FilePath -> IO T.Text #-}
includeFile searchPath name = liftIO $ foldr go (return T.empty) searchPath
    where go dir next = do
            let path = dir </> name
            T.readFile path `E.catch` \(_::IOException) -> next

-- | A problem arose with a template.
data TemplateException =
    TemplateNotFound FilePath   -- ^ The template could not be found.
    deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Exception TemplateException

-- | Load a Mustache template file.
--
-- If the name is an absolute or relative path, the search path is
-- /not/ used, and the name is treated as a literal path.
--
-- This function throws a 'TemplateException' if the template could
-- not be found, or an 'IOException' if no template could be loaded.
loadTemplate :: [FilePath]      -- ^ Search path.
             -> FilePath        -- ^ Name of template file.
             -> IO TL.Text
loadTemplate paths name
    | any isPathSeparator name = TL.readFile name
    | otherwise                = go Nothing paths
  where go me (p:ps) = do
          let cur = p </> name <.> "tpl"
          x <- doesFileExist cur
          if x
            then TL.readFile cur `E.catch` \e -> go (me `mplus` Just e) ps
            else go me ps
        go (Just e) _ = throwIO (e::IOException)
        go _        _ = throwIO . TemplateNotFound $ name
