{-# LANGUAGE CPP, DeriveDataTypeable #-}

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
import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import Data.Typeable (Typeable)

data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show)

data PrintExit = Nada
               | List
               | Version
               | Help
                 deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance Monoid PrintExit where
    mempty  = Nada
    mappend = max

-- | Supported plot outputs.  Some outputs support width and height in
-- varying units.
data PlotOutput = CSV           -- ^ Textual CSV file.
                | PDF Int Int   -- ^ PDF file, dimensions in points.
                | PNG Int Int   -- ^ PNG file, dimensions in pixels.
                | SVG Int Int   -- ^ SVG file, dimensions in points.
                | Window Int Int-- ^ Display in a window, dimensions in pixels.
                  deriving (Eq, Ord, Read, Show)

-- | What to plot.
data Plot = KernelDensity
          | Timing
            deriving (Eq, Ord, Read, Show)

data Config = Config {
      cfgBanner       :: Last String -- ^ The \"version\" banner to print.
    , cfgConfInterval :: Last Double -- ^ Confidence interval to use.
    , cfgPerformGC    :: Last Bool   -- ^ Whether to run the GC between passes.
    , cfgPlot         :: MultiMap Plot PlotOutput -- ^ What to plot, and where.
    , cfgPrintExit    :: PrintExit   -- ^ Whether to print information and exit.
    , cfgResamples    :: Last Int    -- ^ Number of resamples to perform.
    , cfgSamples      :: Last Int    -- ^ Number of samples to collect.
    , cfgVerbosity    :: Last Verbosity -- ^ Whether to run verbosely.
    } deriving (Eq, Read, Show, Typeable)

emptyConfig :: Config
emptyConfig = Config {
                cfgBanner       = mempty
              , cfgConfInterval = mempty
              , cfgPerformGC    = mempty
              , cfgPlot         = mempty
              , cfgPrintExit    = mempty
              , cfgResamples    = mempty
              , cfgSamples      = mempty
              , cfgVerbosity    = mempty
              }

-- | A configuration with sensible defaults.
defaultConfig :: Config
defaultConfig = Config {
                  cfgBanner       = ljust "I don't know what version I am."
                , cfgConfInterval = ljust 0.95
                , cfgPerformGC    = ljust False
                , cfgPlot         = mempty
                , cfgPrintExit    = Nada
                , cfgResamples    = ljust (100 * 1000)
                , cfgSamples      = ljust 100
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

appendConfig :: Config -> Config -> Config
appendConfig a b =
    Config {
      cfgBanner       = app cfgBanner a b
    , cfgConfInterval = app cfgConfInterval a b
    , cfgPerformGC    = app cfgPerformGC a b
    , cfgPlot         = app cfgPlot a b
    , cfgPrintExit    = app cfgPrintExit a b
    , cfgSamples      = app cfgSamples a b
    , cfgResamples    = app cfgResamples a b
    , cfgVerbosity    = app cfgVerbosity a b
    }
  where app f = mappend `on` f

instance Monoid Config where
    mempty  = emptyConfig
    mappend = appendConfig
