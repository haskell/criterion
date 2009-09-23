{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Criterion.Config
    (
      module Data.Monoid
    , Config(..)
    , PlotType(..)
    , Plot(..)
    , PrintExit(..)
    , Verbosity(..)
    , defaultConfig
    , fromLJ
    , ljust
    ) where

import Data.Monoid
import Data.Typeable
import Data.Function
import Data.Set

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

data PlotType = CSV
              | PDF
              | PNG
              | SVG
              | Window
                deriving (Eq, Ord, Bounded, Enum, Read, Show)

data Plot = KernelDensity PlotType
          | Timing PlotType
            deriving (Eq, Ord, Read, Show)

data Config = Config {
      cfgBanner       :: Last String
    , cfgConfInterval :: Last Double
    , cfgPerformGC    :: Last Bool
    , cfgPlot         :: Set Plot
    , cfgPrintExit    :: PrintExit
    , cfgResamples    :: Last Int
    , cfgSamples      :: Last Int
    , cfgVerbosity    :: Verbosity
    } deriving (Eq, Read, Show, Typeable)

instance Monoid Bool where
    mempty  = False
    mappend = (||)

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
                , cfgPlot         = empty
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
