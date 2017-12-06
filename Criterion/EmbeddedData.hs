{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Criterion.EmbeddedData
-- Copyright   : (c) 2017 Ryan Scott
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- When the @embed-data-files@ @Cabal@ flag is enabled, this module exports
-- the contents of various files (the @data-files@ from @criterion.cabal@, as
-- well as minimized versions of jQuery and Flot) embedded as 'ByteString's.
module Criterion.EmbeddedData
  ( dataFiles
  , jQueryContents
  , flotContents
  , flotErrorbarsContents
  , flotNavigateContents
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile)
import Language.Haskell.TH.Syntax (runIO)
import qualified Language.Javascript.Flot as Flot
import qualified Language.Javascript.JQuery as JQuery

dataFiles :: [(FilePath, ByteString)]
dataFiles = $(embedDir "templates")

jQueryContents, flotContents,
  flotErrorbarsContents, flotNavigateContents :: ByteString
jQueryContents        = $(embedFile =<< runIO JQuery.file)
flotContents          = $(embedFile =<< runIO (Flot.file Flot.Flot))
flotErrorbarsContents = $(embedFile =<< runIO (Flot.file Flot.FlotErrorbars))
flotNavigateContents  = $(embedFile =<< runIO (Flot.file Flot.FlotNavigate))
