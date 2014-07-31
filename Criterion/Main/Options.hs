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

import Control.Monad (when)
import Criterion.Analysis (validateAccessors)
import Criterion.Types (Config(..), Verbosity(..))
import Data.Char (isSpace, toLower)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Monoid (mempty)
import GHC.Generics (Generic)
import Options.Applicative
import Options.Applicative.Types

-- | How to match a benchmark name.
data MatchType = Prefix | Glob
               deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                         Generic)

-- | Execution mode for a benchmark program.
data Mode = List
            -- ^ List all benchmarks.
          | OnlyRun Int64 MatchType [String]
            -- ^ Run the given benchmarks, without collecting or
            -- analysing performance numbers.
          | Run Config MatchType [String]
            -- ^ Run and analyse the given benchmarks.
          deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Default benchmarking configuration.
defaultConfig :: Config
defaultConfig = Config {
      confInterval = 0.95
    , forceGC      = True
    , timeLimit    = 5
    , resamples    = 1000
    , regressions  = []
    , rawDataFile  = Nothing
    , reportFile   = Nothing
    , csvFile      = Nothing
    , junitFile    = Nothing
    , verbosity    = Normal
    , template     = "default"
    }

parseWith :: Config -> Parser Mode
parseWith cfg =
    (matchNames (Run <$> config cfg)) <|>
    onlyRun <|>
    (List <$ switch (long "list" <> short 'l' <> help "list benchmarks"))
  where
    onlyRun = matchNames $
      OnlyRun <$> option (long "only-run" <> short 'n' <> metavar "ITERS" <>
                          help "run benchmarks, don't analyse")
    matchNames wat = wat
      <*> option (long "match" <> short 'm' <> metavar "MATCH" <>
                  value Prefix <> reader match <>
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
  <*> option (long "resamples" <> metavar "COUNT" <> value resamples <>
              reader (range 1 1000000) <>
              help "number of bootstrap resamples to perform")
  <*> many (option (long "regress" <> metavar "RESP:PRED.." <>
                    reader regressParams <>
                    help "regressions to perform"))
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

regressParams :: String -> ReadM ([String], String)
regressParams m = do
  let repl ','   = ' '
      repl c     = c
      tidy       = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      (r,ps)     = break (==':') m
  when (null r) $
    readerError "no responder specified"
  when (null ps) $
    readerError "no predictors specified"
  let ret = (words . map repl . drop 1 $ ps, tidy r)
  either readerError (ReadM . Right . const ()) $ uncurry validateAccessors ret
  return ret

describe :: Config -> ParserInfo Mode
describe cfg = info (helper <*> parseWith cfg) $
  fullDesc
