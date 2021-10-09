{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

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
import Data.Aeson (ToJSON (..), Value(..), object, (.=), Value)
import Data.Aeson.Text (encodeToLazyText)
import Data.Data (Data, Typeable)
import Data.Foldable (forM_)
import GHC.Generics (Generic)
import Paths_criterion (getDataFileName)
import Statistics.Function (minMax)
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>), isPathSeparator)
import System.IO (hPutStrLn, stderr)
import Text.Microstache (Key (..), Node (..), Template (..),
                compileMustacheText, displayMustacheWarning, renderMustacheW)
import Prelude ()
import Prelude.Compat
import qualified Control.Exception as E
import qualified Data.Text as T
#if defined(EMBED)
import qualified Data.Text.Lazy.Encoding as TLE
#endif
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

#if defined(EMBED)
import Criterion.EmbeddedData (dataFiles, chartContents)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
#else
import qualified Language.Javascript.Chart as Chart
#endif

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
#endif

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
--
-- When the @-fembed-data-files@ @Cabal@ flag is enabled, this simply
-- returns the empty path.
getTemplateDir :: IO FilePath
#if defined(EMBED)
getTemplateDir = pure ""
#else
getTemplateDir = getDataFileName "templates"
#endif

-- | Write out a series of 'Report' values to a single file, if
-- configured to do so.
report :: [Report] -> Criterion ()
report reports = do
  Config{..} <- ask
  forM_ reportFile $ \name -> liftIO $ do
    td <- getTemplateDir
    tpl <- loadTemplate [td,"."] template
    TL.writeFile name =<< formatReport reports tpl

-- | Escape JSON string aimed to be embedded in an HTML <script> tag.  Notably
-- < and > are replaced with their unicode escape sequences such that closing
-- the <script> tag from within the JSON data is disallowed, i.e, the character
-- sequence "</" is made impossible.
--
-- Moreover, single quotes are escaped such that embedding JSON into HTML
-- attributes quoted with single quotes is safe, & is escaped to avoid HTML
-- character references (&<code>;) and + is escaped to avoid UTF-7 attacks
-- (should only affect old versions of IE).
--
-- The following characters are replaced with their unicode escape sequnces
-- (\uXXXX) <, >, &, +, \0, \n, \r, ' (single quote), /, \, \x2028 (line
-- separator) and \x2029 (paragraph separator)
escapeJSON :: Char -> TL.Text
escapeJSON '<'      = "\\u003c" -- ban closing of the script tag by making </ impossible
escapeJSON '>'      = "\\u003e" -- encode tags with unicode escape sequences
escapeJSON '\x2028' = "\\u2028" -- line separator
escapeJSON '\x2029' = "\\u2029" -- paragraph separator
escapeJSON '&'      = "\\u0026" -- avoid HTML entities
escapeJSON '+'      = "\\u002b" -- + can be used in UTF-7 escape sequences
escapeJSON '\0'     = "\\u0000" -- make null characters explicit
escapeJSON '\n'     = "\\u000a" -- for good measure also escape newlines
escapeJSON '\r'     = "\\u000d" -- , carriage returns
escapeJSON '\''     = "\\u0027" -- , single quotes
escapeJSON '/'      = "\\u002f" -- , slashes
escapeJSON '\\'     = "\\u005c" -- , and backslashes
escapeJSON c        = TL.singleton c

-- | Format a series of 'Report' values using the given Mustache template.
formatReport :: [Report]
             -> TL.Text    -- ^ Mustache template.
             -> IO TL.Text
formatReport reports templateName = do
    template0 <- case compileMustacheText "tpl" templateName of
        Left err -> fail (show err) -- TODO: throw a template exception?
        Right x -> return x

    criterionJS <- readDataFile "criterion.js"
    criterionCSS <- readDataFile "criterion.css"
    chartJS <- chartFileContents

    -- includes, only top level
    templates <- getTemplateDir
    template <- includeTemplate (includeFile [templates]) template0

    let context = object
            [ "json"                .= reportsJSON reports
            , "js-criterion"        .= criterionJS
            , "js-chart"            .= chartJS
            , "criterion-css"       .= criterionCSS
            ]

    let (warnings, formatted) = renderMustacheW template context
    -- If there were any issues during mustache template rendering, make sure
    -- to inform the user. See #127.
    forM_ warnings $ \warning -> do
        criterionWarning $ displayMustacheWarning warning
    return formatted
  where
    reportsJSON :: [Report] -> T.Text
    reportsJSON = TL.toStrict . TL.concatMap escapeJSON . encodeToLazyText

    chartFileContents :: IO T.Text
#if defined(EMBED)
    chartFileContents        = pure $ TE.decodeUtf8 chartContents
#else
    chartFileContents        = T.readFile =<< Chart.file Chart.Chart
#endif

    readDataFile :: FilePath -> IO T.Text
    readDataFile fp =
      (T.readFile =<< getDataFileName ("templates" </> fp))
#if defined(EMBED)
      `E.catch` \(e :: IOException) ->
        maybe (throwIO e)
              (pure . TE.decodeUtf8)
              (lookup fp dataFiles)
#endif

    includeTemplate :: (FilePath -> IO T.Text) -> Template -> IO Template
    includeTemplate f Template {..} = fmap
        (Template templateActual)
        (traverse (traverse (includeNode f)) templateCache)

    includeNode :: (FilePath -> IO T.Text) -> Node -> IO Node
    includeNode f (Section (Key ["include"]) [TextBlock fp]) =
        fmap TextBlock (f (T.unpack fp))
    includeNode _ n = return n

criterionWarning :: String -> IO ()
criterionWarning msg =
  hPutStrLn stderr $ unlines
    [ "criterion: warning:"
    , "  " ++ msg
    ]

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
    val i = object [ toKey name .= i ]


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
        [ toKey name1 .= i
        , toKey name2 .= j
        ]

#if MIN_VERSION_aeson(2,0,0)
toKey :: T.Text -> Key.Key
toKey = Key.fromText
#else
toKey :: T.Text -> T.Text
toKey = id
#endif


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
-- If the @-fembed-data-files@ @Cabal@ flag is enabled, this also checks
-- the embedded @data-files@ from @criterion.cabal@.
--
-- This function throws a 'TemplateException' if the template could
-- not be found, or an 'IOException' if no template could be loaded.
loadTemplate :: [FilePath]      -- ^ Search path.
             -> FilePath        -- ^ Name of template file.
             -> IO TL.Text
loadTemplate paths name
    | any isPathSeparator name = readFileCheckEmbedded name
    | otherwise                = go Nothing paths
  where go me (p:ps) = do
          let cur = p </> name <.> "tpl"
          x <- doesFileExist' cur
          if x
            then readFileCheckEmbedded cur `E.catch` \e -> go (me `mplus` Just e) ps
            else go me ps
        go (Just e) _ = throwIO (e::IOException)
        go _        _ = throwIO . TemplateNotFound $ name

        doesFileExist' :: FilePath -> IO Bool
        doesFileExist' fp = do
          e <- doesFileExist fp
          pure $ e
#if defined(EMBED)
                 || (fp `elem` map fst dataFiles)
#endif

-- A version of 'readFile' that falls back on the embedded 'dataFiles'
-- from @criterion.cabal@.
readFileCheckEmbedded :: FilePath -> IO TL.Text
readFileCheckEmbedded fp =
  TL.readFile fp
#if defined(EMBED)
  `E.catch` \(e :: IOException) ->
    maybe (throwIO e)
          (pure . TLE.decodeUtf8 . fromStrict)
          (lookup fp dataFiles)
  where
# if MIN_VERSION_bytestring(0,10,0)
    fromStrict = BL.fromStrict
# else
    fromStrict x = BL.fromChunks [x]
# endif
#endif
