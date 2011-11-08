{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Criterion.Config
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarking configuration.

module Criterion.Config
    (
      Config(..)
    , PlotOutput(..)
    , Plot(..)
    , PrintExit(..)
    , Verbosity(..)
    , defaultConfig
    , fromLJ
    , ljust
    ) where

import Criterion.MultiMap (MultiMap)
import Data.Data (Data)
import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import Data.Typeable (Typeable)

-- | Control the amount of information displayed.
data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable)

-- | Print some information and exit, without running any benchmarks.
data PrintExit = Nada           -- ^ Do not actually print-and-exit. (Default.)
               | List           -- ^ Print a list of known benchmarks.
               | Version        -- ^ Print version information (if known).
               | Help           -- ^ Print a help\/usaage message.
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data)

instance Monoid PrintExit where
    mempty  = Nada
    mappend = max

-- | Supported plot outputs.  Some outputs support width and height in
-- varying units.  A point is 1\/72 of an inch (0.353mm).
data PlotOutput = CSV           -- ^ Textual CSV file.
                | HTML          -- ^ HTML report.
                | PDF Int Int   -- ^ PDF file, dimensions in points.
                | PNG Int Int   -- ^ PNG file, dimensions in pixels.
                | SVG Int Int   -- ^ SVG file, dimensions in points.
                | Window Int Int-- ^ Display in a window, dimensions in pixels.
                  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | What to plot.
data Plot = KernelDensity       -- ^ Kernel density estimate of probabilities.
          | Timing              -- ^ Benchmark timings.
            deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | Top-level program configuration.
data Config = Config {
      cfgBanner       :: Last String -- ^ The \"version\" banner to print.
    , cfgConfInterval :: Last Double -- ^ Confidence interval to use.
    , cfgPerformGC    :: Last Bool   -- ^ Whether to run the GC between passes.
    , cfgPlot         :: MultiMap Plot PlotOutput -- ^ What to plot, and where.
    , cfgPlotSameAxis :: Last Bool
    , cfgPrintExit    :: PrintExit   -- ^ Whether to print information and exit.
    , cfgResamples    :: Last Int    -- ^ Number of resamples to perform.
    , cfgSamples      :: Last Int    -- ^ Number of samples to collect.
    , cfgSummaryFile  :: Last FilePath -- ^ Filename of summary CSV
    , cfgVerbosity    :: Last Verbosity -- ^ Whether to run verbosely.
    } deriving (Eq, Read, Show, Typeable)

instance Monoid Config where
    mempty  = emptyConfig
    mappend = appendConfig

-- | A configuration with sensible defaults.
defaultConfig :: Config
defaultConfig = Config {
                  cfgBanner       = ljust "I don't know what version I am."
                , cfgConfInterval = ljust 0.95
                , cfgPerformGC    = ljust False
                , cfgPlot         = mempty
                , cfgPlotSameAxis = ljust False
                , cfgPrintExit    = Nada
                , cfgResamples    = ljust (100 * 1000)
                , cfgSamples      = ljust 100
                , cfgSummaryFile  = mempty
                , cfgVerbosity    = ljust Normal
                }

-- | Constructor for 'Last' values.
ljust :: a -> Last a
ljust = Last . Just

-- | Deconstructor for 'Last' values.
fromLJ :: (Config -> Last a)    -- ^ Field to access.
       -> Config                -- ^ Default to use.
       -> a
fromLJ f cfg = case f cfg of
                 Last Nothing  -> fromLJ f defaultConfig
                 Last (Just a) -> a

emptyConfig :: Config
emptyConfig = Config {
                cfgBanner       = mempty
              , cfgConfInterval = mempty
              , cfgPerformGC    = mempty
              , cfgPlot         = mempty
              , cfgPlotSameAxis = mempty
              , cfgPrintExit    = mempty
              , cfgResamples    = mempty
              , cfgSamples      = mempty
              , cfgSummaryFile  = mempty
              , cfgVerbosity    = mempty
              }

appendConfig :: Config -> Config -> Config
appendConfig a b =
    Config {
      cfgBanner       = app cfgBanner a b
    , cfgConfInterval = app cfgConfInterval a b
    , cfgPerformGC    = app cfgPerformGC a b
    , cfgPlot         = app cfgPlot a b
    , cfgPlotSameAxis = app cfgPlotSameAxis a b
    , cfgPrintExit    = app cfgPrintExit a b
    , cfgSamples      = app cfgSamples a b
    , cfgSummaryFile  = app cfgSummaryFile a b
    , cfgResamples    = app cfgResamples a b
    , cfgVerbosity    = app cfgVerbosity a b
    }
  where app f = mappend `on` f
