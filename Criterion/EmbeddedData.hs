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
-- well as a minimized version of Chart.js) embedded as a 'ByteString'.
module Criterion.EmbeddedData
  ( dataFiles
  , chartContents
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile)
import Language.Haskell.TH.Syntax (runIO)
import qualified Language.Javascript.Chart as Chart

dataFiles :: [(FilePath, ByteString)]
dataFiles = $(embedDir "templates")

chartContents :: ByteString
chartContents = $(embedFile =<< runIO (Chart.file Chart.Chart))
