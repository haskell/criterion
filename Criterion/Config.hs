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

instance Monoid Verbosity where
    mempty        = Normal
    _ `mappend` b = b
                   
data PrintExit = Nada
               | Version
               | Help
                 deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance Monoid PrintExit where
    mempty  = Nada
    mappend = max

data PlotOutput = CSV
                | PDF
                | PNG
                | SVG
                | Window
                  deriving (Eq, Ord, Bounded, Enum, Read, Show)

data Plot = KernelDensity
          | Timing
            deriving (Eq, Ord, Read, Show)

data Config = Config {
      cfgBanner       :: Last String
    , cfgConfInterval :: Last Double
    , cfgPerformGC    :: Last Bool
    , cfgPlot         :: MultiMap Plot PlotOutput
    , cfgPrintExit    :: PrintExit
    , cfgResamples    :: Last Int
    , cfgSamples      :: Last Int
    , cfgVerbosity    :: Verbosity
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

defaultConfig :: Config
defaultConfig = Config {
                  cfgBanner       = ljust "hi mom!"
                , cfgConfInterval = ljust 0.95
                , cfgPerformGC    = ljust False
                , cfgPlot         = mempty
                , cfgPrintExit    = Nada
                , cfgResamples    = ljust (100 * 1000)
                , cfgSamples      = ljust 100
                , cfgVerbosity    = Normal
                }

ljust :: a -> Last a
ljust = Last . Just

fromLJ :: (Config -> Last a) -> Config -> a
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
