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
    , versionInfo
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Data.Monoid 

import Control.Monad (when)
import Criterion.Analysis (validateAccessors)
import Criterion.Types (Config(..), Verbosity(..), measureAccessors,
                        measureKeys)
import Data.Char (isSpace, toLower)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative
import Options.Applicative.Help (Chunk(..), tabulate)
import Options.Applicative.Help.Pretty ((.$.))
import Options.Applicative.Types
import Paths_criterion (version)
import Text.PrettyPrint.ANSI.Leijen (Doc, text)
import qualified Data.Map as M
import Prelude

-- | How to match a benchmark name.
data MatchType = Prefix
                 -- ^ Match by prefix. For example, a prefix of
                 -- @\"foo\"@ will match @\"foobar\"@.
               | Glob
                 -- ^ Match by Unix-style glob pattern.
               deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                         Generic)

-- | Execution mode for a benchmark program.
data Mode = List
            -- ^ List all benchmarks.
          | Version
            -- ^ Print the version.
          | RunIters Int64 MatchType [String]
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
    , jsonFile     = Nothing
    , junitFile    = Nothing
    , verbosity    = Normal
    , template     = "default"
    }

-- | Parse a command line.
parseWith :: Config
             -- ^ Default configuration to use if options are not
             -- explicitly specified.
          -> Parser Mode
parseWith cfg =
    (matchNames (Run <$> config cfg)) <|>
    runIters <|>
    (List <$ switch (long "list" <> short 'l' <> help "List benchmarks")) <|>
    (Version <$ switch (long "version" <> help "Show version info"))
  where
    runIters = matchNames $
      RunIters <$> option auto
                  (long "iters" <> short 'n' <> metavar "ITERS" <>
                   help "Run benchmarks, don't analyse")
    matchNames wat = wat
      <*> option match
          (long "match" <> short 'm' <> metavar "MATCH" <> value Prefix <>
           help "How to match benchmark names (\"prefix\" or \"glob\")")
      <*> many (argument str (metavar "NAME..."))

config :: Config -> Parser Config
config Config{..} = Config
  <$> option (range 0.001 0.999)
      (long "ci" <> short 'I' <> metavar "CI" <> value confInterval <>
       help "Confidence interval")
  <*> (not <$> switch (long "no-gc" <> short 'G' <>
                       help "Do not collect garbage between iterations"))
  <*> option (range 0.1 86400)
      (long "time-limit" <> short 'L' <> metavar "SECS" <> value timeLimit <>
       help "Time limit to run a benchmark")
  <*> option (range 1 1000000)
      (long "resamples" <> metavar "COUNT" <> value resamples <>
       help "Number of bootstrap resamples to perform")
  <*> many (option regressParams
            (long "regress" <> metavar "RESP:PRED.." <>
             help "Regressions to perform"))
  <*> outputOption rawDataFile (long "raw" <>
                                help "File to write raw data to")
  <*> outputOption reportFile (long "output" <> short 'o' <>
                               help "File to write report to")
  <*> outputOption csvFile (long "csv" <>
                            help "File to write CSV summary to")
  <*> outputOption jsonFile (long "json" <>
                             help "File to write JSON summary to")
  <*> outputOption junitFile (long "junit" <>
                              help "File to write JUnit summary to")
  <*> (toEnum <$> option (range 0 2)
                  (long "verbosity" <> short 'v' <> metavar "LEVEL" <>
                   value (fromEnum verbosity) <>
                   help "Verbosity level"))
  <*> strOption (long "template" <> short 't' <> metavar "FILE" <>
                 value template <>
                 help "Template to use for report")

outputOption :: Maybe String -> Mod OptionFields String -> Parser (Maybe String)
outputOption file m =
  optional (strOption (m <> metavar "FILE" <> maybe mempty value file))

range :: (Show a, Read a, Ord a) => a -> a -> ReadM a
range lo hi = do
  s <- readerAsk
  case reads s of
    [(i, "")]
      | i >= lo && i <= hi -> return i
      | otherwise -> readerError $ show i ++ " is outside range " ++
                                   show (lo,hi)
    _             -> readerError $ show s ++ " is not a number"

match :: ReadM MatchType
match = do
  m <- readerAsk
  case map toLower m of
    mm | mm `isPrefixOf` "pfx"    -> return Prefix
       | mm `isPrefixOf` "prefix" -> return Prefix
       | mm `isPrefixOf` "glob"   -> return Glob
       | otherwise                ->
         readerError $ show m ++ " is not a known match type. "
                              ++ "Try \"prefix\" or \"glob\"."

regressParams :: ReadM ([String], String)
regressParams = do
  m <- readerAsk
  let repl ','   = ' '
      repl c     = c
      tidy       = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      (r,ps)     = break (==':') m
  when (null r) $
    readerError "no responder specified"
  when (null ps) $
    readerError "no predictors specified"
  let ret = (words . map repl . drop 1 $ ps, tidy r)
  either readerError (const (return ret)) $ uncurry validateAccessors ret

-- | Flesh out a command line parser.
describe :: Config -> ParserInfo Mode
describe cfg = info (helper <*> parseWith cfg) $
    header ("Microbenchmark suite - " <> versionInfo) <>
    fullDesc <>
    footerDoc (unChunk regressionHelp)

-- | A string describing the version of this benchmark (really, the
-- version of criterion that was used to build it).
versionInfo :: String
versionInfo = "built with criterion " <> showVersion version

-- We sort not by name, but by likely frequency of use.
regressionHelp :: Chunk Doc
regressionHelp =
    fmap (text "Regression metrics (for use with --regress):" .$.) $
      tabulate [(text n,text d) | (n,(_,d)) <- map f measureKeys]
  where f k = (k, measureAccessors M.! k)
