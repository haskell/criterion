{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

-- |
-- Module      : Criterion.Main.Options
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarking command-line configuration.

module Criterion.Main.Options
    (
      Mode(..)
    , MatchType(..)
    , defaultConfig
    , parseWith
    , describe
    ) where

import Criterion.Types (Config(..), Verbosity(..))
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.List (isPrefixOf)
import Data.Monoid (mempty)
import Options.Applicative

-- | How to match a benchmark name.
data MatchType = Prefix | Glob
               deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                         Generic)

data Mode = List
          | Run Config MatchType [String]
          deriving (Eq, Read, Show, Typeable, Data, Generic)

defaultConfig :: Config
defaultConfig = Config {
      confInterval = 0.95
    , forceGC      = True
    , timeLimit    = 5
    , onlyRun      = Nothing
    , resamples    = 10000
    , rawDataFile  = Nothing
    , reportFile   = Nothing
    , csvFile      = Nothing
    , junitFile    = Nothing
    , verbosity    = Normal
    , template     = "default"
    }

parseWith :: Config -> Parser Mode
parseWith cfg =
  run cfg <|>
  (List <$ switch (long "list" <> short 'l' <> help "list benchmarks"))

run :: Config -> Parser Mode
run cfg = Run
  <$> config cfg
  <*> option (long "match" <> short 'm' <> metavar "MATCH" <> value Prefix <>
              reader match <>
              help "how to match benchmark names")
  <*> many (argument str (metavar "NAME..."))

config :: Config -> Parser Config
config Config{..} = Config
  <$> option (long "ci" <> short 'I' <> metavar "CI" <> value confInterval <>
              reader (range 0.001 0.999) <>
              help "confidence interval")
  <*> (not <$> switch (long "no-gc" <> short 'G' <>
                       help "do not collect garbage between iterations"))
  <*> option (long "time-limit" <> short 'L' <> metavar "SECS" <>
              value timeLimit <> reader (range 0.1 86400) <>
              help "time limit to run a benchmark")
  <*> optional
      (option (long "only-run" <> short 'n' <> metavar "ITERS" <>
               help "run benchmarks, don't analyse"))
  <*> option (long "resamples" <> metavar "COUNT" <> value resamples <>
              reader (range 1 1000000) <>
              help "number of bootstrap resamples to perform")
  <*> outputOption rawDataFile (long "raw" <>
                                help "file to write raw data to")
  <*> outputOption reportFile (long "output" <> short 'o' <>
                               help "file to write report to")
  <*> outputOption csvFile (long "csv" <>
                            help "file to write CSV summary to")
  <*> outputOption junitFile (long "junit" <>
                              help "file to write JUnit summary to")
  <*> (toEnum <$> option (long "verbosity" <> short 'v' <> metavar "LEVEL" <>
                          value (fromEnum verbosity) <> reader (range 0 2) <>
                          help "verbosity level"))
  <*> strOption (long "template" <> short 't' <> metavar "FILE" <>
                 value template <>
                 help "template to use for report")

outputOption :: Maybe String -> Mod OptionFields String -> Parser (Maybe String)
outputOption file m =
  optional (strOption (m <> metavar "FILE" <> maybe mempty value file))

range :: (Show a, Read a, Ord a) => a -> a -> String -> ReadM a
range lo hi s =
    case reads s of
      [(i, "")]
        | i >= lo && i <= hi -> return i
        | otherwise -> readerError $ show i ++ " is outside range " ++
                                     show (lo,hi)
      _             -> readerError $ show s ++ " is not a number"

match :: String -> ReadM MatchType
match m
    | mm `isPrefixOf` "pfx"    = return Prefix
    | mm `isPrefixOf` "prefix" = return Prefix
    | mm `isPrefixOf` "glob"   = return Glob
    | otherwise                = readerError $
                                 show m ++ " is not a known match type"
    where mm = map toLower m

describe :: Config -> ParserInfo Mode
describe cfg = info (helper <*> parseWith cfg) $
  fullDesc
