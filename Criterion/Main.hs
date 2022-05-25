{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Criterion.Main
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Wrappers for compiling and running benchmarks quickly and easily.
-- See 'defaultMain' below for an example.
--
-- All of the 'IO'-returning functions in this module initialize the timer
-- before measuring time (refer to the documentation for 'initializeTime'
-- for more details).

module Criterion.Main
    (
    -- * How to write benchmarks
    -- $bench

    -- ** Benchmarking IO actions
    -- $io

    -- ** Benchmarking pure code
    -- $pure

    -- ** Fully evaluating a result
    -- $rnf

    -- * Types
      Benchmarkable
    , Benchmark
    -- * Creating a benchmark suite
    , env
    , envWithCleanup
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup
    , toBenchmarkable
    , bench
    , bgroup
    -- ** Running a benchmark
    , nf
    , whnf
    , nfIO
    , whnfIO
    , nfAppIO
    , whnfAppIO
#if MIN_VERSION_base(4,16,0)
    , nfLinear
    , whnfLinear
    , nfAppIoLinear
    , whnfAppIoLinear
#endif
    -- * Turning a suite of benchmarks into a program
    , defaultMain
    , defaultMainWith
    , defaultConfig
    -- * Other useful code
    , makeMatcher
    , runMode
    ) where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Criterion.IO.Printf (printError, writeCsv)
import Criterion.Internal (runAndAnalyse, runFixedIters)
import Criterion.Main.Options (MatchType(..), Mode(..), defaultConfig, describe,
                               versionInfo)
import Criterion.Measurement (initializeTime)
import Criterion.Monad (withConfig)
import Criterion.Types
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe)
import Options.Applicative (execParser)
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath.Glob

-- | An entry point that can be used as a @main@ function.
--
-- > import Criterion.Main
-- >
-- > fib :: Int -> Int
-- > fib 0 = 0
-- > fib 1 = 1
-- > fib n = fib (n-1) + fib (n-2)
-- >
-- > main = defaultMain [
-- >        bgroup "fib" [ bench "10" $ whnf fib 10
-- >                     , bench "35" $ whnf fib 35
-- >                     , bench "37" $ whnf fib 37
-- >                     ]
-- >                    ]
defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig

-- | Create a function that can tell if a name given on the command
-- line matches a benchmark.
makeMatcher :: MatchType
            -> [String]
            -- ^ Command line arguments.
            -> Either String (String -> Bool)
makeMatcher matchKind args =
  case matchKind of
    Prefix -> Right $ \b -> null args || any (`isPrefixOf` b) args
    Glob ->
      let compOptions = compDefault { errorRecovery = False }
      in case mapM (tryCompileWith compOptions) args of
           Left errMsg -> Left . fromMaybe errMsg . stripPrefix "compile :: " $
                          errMsg
           Right ps -> Right $ \b -> null ps || any (`match` b) ps
    Pattern -> Right $ \b -> null args || any (`isInfixOf` b) args
    IPattern -> Right $ \b -> null args || any (`isInfixOf` map toLower b) (map (map toLower) args)

selectBenches :: MatchType -> [String] -> Benchmark -> IO (String -> Bool)
selectBenches matchType benches bsgroup = do
  toRun <- either parseError return . makeMatcher matchType $ benches
  unless (null benches || any toRun (benchNames bsgroup)) $
    parseError "none of the specified names matches a benchmark"
  return toRun

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
--
-- Example:
--
-- > import Criterion.Main.Options
-- > import Criterion.Main
-- >
-- > myConfig = defaultConfig {
-- >               -- Resample 10 times for bootstrapping
-- >               resamples = 10
-- >            }
-- >
-- > main = defaultMainWith myConfig [
-- >          bench "fib 30" $ whnf fib 30
-- >        ]
--
-- If you save the above example as @\"Fib.hs\"@, you should be able
-- to compile it as follows:
--
-- > ghc -O --make Fib
--
-- Run @\"Fib --help\"@ on the command line to get a list of command
-- line options.
defaultMainWith :: Config
                -> [Benchmark]
                -> IO ()
defaultMainWith defCfg bs = do
  wat <- execParser (describe defCfg)
  runMode wat bs

-- | Run a set of 'Benchmark's with the given 'Mode'.
--
-- This can be useful if you have a 'Mode' from some other source (e.g. from a
-- one in your benchmark driver's command-line parser).
runMode :: Mode -> [Benchmark] -> IO ()
runMode wat bs =
  case wat of
    List -> mapM_ putStrLn . sort . concatMap benchNames $ bs
    Version -> putStrLn versionInfo
    RunIters cfg iters matchType benches -> do
      shouldRun <- selectBenches matchType benches bsgroup
      withConfig cfg $
        runFixedIters iters shouldRun bsgroup
    Run cfg matchType benches -> do
      shouldRun <- selectBenches matchType benches bsgroup
      withConfig cfg $ do
        writeCsv ("Name","Mean","MeanLB","MeanUB","Stddev","StddevLB",
                  "StddevUB")
        liftIO initializeTime
        runAndAnalyse shouldRun bsgroup
  where bsgroup = BenchGroup "" bs

-- | Display an error message from a command line parsing failure, and
-- exit.
parseError :: String -> IO a
parseError msg = do
  _ <- printError "Error: %s\n" msg
  _ <- printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

-- $bench
--
-- The 'Benchmarkable' type is a container for code that can be
-- benchmarked.  The value inside must run a benchmark the given
-- number of times.  We are most interested in benchmarking two
-- things:
--
-- * 'IO' actions.  Most 'IO' actions can be benchmarked directly.
--
-- * Pure functions.  GHC optimises aggressively when compiling with
--   @-O@, so it is easy to write innocent-looking benchmark code that
--   doesn't measure the performance of a pure function at all.  We
--   work around this by benchmarking both a function and its final
--   argument together.

-- $io
--
-- Most 'IO' actions can be benchmarked easily using one of the following
-- two functions:
--
-- @
-- 'nfIO'   :: 'NFData' a => 'IO' a -> 'Benchmarkable'
-- 'whnfIO' ::               'IO' a -> 'Benchmarkable'
-- @
--
-- In certain corner cases, you may find it useful to use the following
-- variants, which take the input as a separate argument:
--
-- @
-- 'nfAppIO'   :: 'NFData' b => (a -> 'IO' b) -> a -> 'Benchmarkable'
-- 'whnfAppIO' ::               (a -> 'IO' b) -> a -> 'Benchmarkable'
-- @
--
-- This is useful when the bulk of the work performed by the function is
-- not bound by IO, but rather by pure computations that may optimize away if
-- the argument is known statically, as in 'nfIO'/'whnfIO'.

-- $pure
--
-- Because GHC optimises aggressively when compiling with @-O@, it is
-- potentially easy to write innocent-looking benchmark code that will
-- only be evaluated once, for which all but the first iteration of
-- the timing loop will be timing the cost of doing nothing.
--
-- To work around this, we provide two functions for benchmarking pure
-- code.
--
-- The first will cause results to be fully evaluated to normal form
-- (NF):
--
-- @
-- 'nf' :: 'NFData' b => (a -> b) -> a -> 'Benchmarkable'
-- @
--
-- The second will cause results to be evaluated to weak head normal
-- form (the Haskell default):
--
-- @
-- 'whnf' :: (a -> b) -> a -> 'Benchmarkable'
-- @
--
-- As both of these types suggest, when you want to benchmark a
-- function, you must supply two values:
--
-- * The first element is the function, saturated with all but its
--   last argument.
--
-- * The second element is the last argument to the function.
--
-- Here is an example that makes the use of these functions clearer.
-- Suppose we want to benchmark the following function:
--
-- @
-- firstN :: Int -> [Int]
-- firstN k = take k [(0::Int)..]
-- @
--
-- So in the easy case, we construct a benchmark as follows:
--
-- @
-- 'nf' firstN 1000
-- @

-- $rnf
--
-- The 'whnf' harness for evaluating a pure function only evaluates
-- the result to weak head normal form (WHNF).  If you need the result
-- evaluated all the way to normal form, use the 'nf' function to
-- force its complete evaluation.
--
-- Using the @firstN@ example from earlier, to naive eyes it might
-- /appear/ that the following code ought to benchmark the production
-- of the first 1000 list elements:
--
-- @
-- 'whnf' firstN 1000
-- @
--
-- Since we are using 'whnf', in this case the result will only be
-- forced until it reaches WHNF, so what this would /actually/
-- benchmark is merely how long it takes to produce the first list
-- element!
