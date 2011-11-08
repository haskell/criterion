-- |
-- Module      : Criterion.Main
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Wrappers for compiling and running benchmarks quickly and easily.
-- See 'defaultMain' below for an example.

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
      Benchmarkable(..)
    , Benchmark
    , Pure
    -- * Constructing benchmarks
    , bench
    , bgroup
    , nf
    , whnf
    , nfIO
    , whnfIO
    -- * Running benchmarks
    , defaultMain
    , defaultMainWith
    -- * Other useful code
    , defaultOptions
    , parseArgs
    ) where

import Control.Monad.Trans (liftIO)
import Criterion (runAndAnalyse)
import Criterion.Config
import Criterion.Environment (measureEnvironment)
import Criterion.IO (note, printError)
import Criterion.Monad (Criterion, withConfig)
import Criterion.Types (Benchmarkable(..), Benchmark(..), Pure, bench,
                        benchNames, bgroup, nf, nfIO, whnf, whnfIO)
import Data.List (isPrefixOf, sort)
import Data.Monoid (Monoid(..), Last(..))
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)

-- | Parse a confidence interval.
ci :: String -> IO Config
ci s = case reads s' of
         [(d,"%")] -> check (d/100)
         [(d,"")]  -> check d
         _         -> parseError "invalid confidence interval provided"
  where s' = case s of
               ('.':_) -> '0':s
               _       -> s
        check d | d <= 0 = parseError "confidence interval is negative"
                | d >= 1 = parseError "confidence interval is greater than 1"
                | otherwise = return mempty { cfgConfInterval = ljust d }

-- | Parse a positive number.
pos :: (Num a, Ord a, Read a) =>
       String -> (Last a -> Config) -> String -> IO Config
pos q f s =
    case reads s of
      [(n,"")] | n > 0     -> return . f $ ljust n
               | otherwise -> parseError $ q ++ " must be positive"
      _                    -> parseError $ "invalid " ++ q ++ " provided"

noArg :: Config -> ArgDescr (IO Config)
noArg = NoArg . return

-- | The standard options accepted on the command line.
defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
   Option ['h','?'] ["help"] (noArg mempty { cfgPrintExit = Help })
          "print help, then exit"
 , Option ['G'] ["no-gc"] (noArg mempty { cfgPerformGC = ljust False })
          "do not collect garbage between iterations"
 , Option ['g'] ["gc"] (noArg mempty { cfgPerformGC = ljust True })
          "collect garbage between iterations"
 , Option ['I'] ["ci"] (ReqArg ci "CI")
          "bootstrap confidence interval"
 , Option ['l'] ["list"] (noArg mempty { cfgPrintExit = List })
          "print only a list of benchmark names"
 , Option ['q'] ["quiet"] (noArg mempty { cfgVerbosity = ljust Quiet })
          "print less output"
 , Option [] ["resamples"]
          (ReqArg (pos "resample count"$ \n -> mempty { cfgResamples = n }) "N")
          "number of bootstrap resamples to perform"
 , Option ['s'] ["samples"]
          (ReqArg (pos "sample count" $ \n -> mempty { cfgSamples = n }) "N")
          "number of samples to collect"
 , Option ['u'] ["summary"] (ReqArg (\s -> return $ mempty { cfgSummaryFile = ljust s }) "FILENAME")
          "produce a summary CSV file of all results"
 , Option ['V'] ["version"] (noArg mempty { cfgPrintExit = Version })
          "display version, then exit"
 , Option ['v'] ["verbose"] (noArg mempty { cfgVerbosity = ljust Verbose })
          "print more output"
 ]

printBanner :: Config -> IO ()
printBanner cfg = withConfig cfg $ 
    case cfgBanner cfg of
      Last (Just b) -> note "%s\n" b
      _             -> note "Hey, nobody told me what version I am!\n"

printUsage :: [OptDescr (IO Config)] -> ExitCode -> IO a
printUsage options exitCode = do
  p <- getProgName
  putStr (usageInfo ("Usage: " ++ p ++ " [OPTIONS] [BENCHMARKS]") options)
  putStrLn "If no benchmark names are given, all are run\n\
           \Otherwise, benchmarks are run by prefix match"
  exitWith exitCode

-- | Parse command line options.
parseArgs :: Config -> [OptDescr (IO Config)] -> [String]
          -> IO (Config, [String])
parseArgs defCfg options args =
  case getOpt Permute options args of
    (_, _, (err:_)) -> parseError err
    (opts, rest, _) -> do
      cfg <- (mappend defCfg . mconcat) `fmap` sequence opts
      case cfgPrintExit cfg of
        Help ->    printBanner cfg >> printUsage options ExitSuccess
        Version -> printBanner cfg >> exitWith ExitSuccess
        _ ->       return (cfg, rest)

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
defaultMain = defaultMainWith defaultConfig (return ())

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
--
-- Example:
--
-- > import Criterion.Config
-- > import qualified Criterion.MultiMap as M
-- > import Criterion.Main
-- >
-- > myConfig = defaultConfig {
-- >              -- Always GC between runs.
-- >              cfgPerformGC = ljust True
-- >            }
-- > 
-- > main = defaultMainWith myConfig (return ()) [
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
                -> Criterion () -- ^ Prepare data prior to executing the first benchmark.
                -> [Benchmark]
                -> IO ()
defaultMainWith defCfg prep bs = do
  (cfg, args) <- parseArgs defCfg defaultOptions =<< getArgs
  withConfig cfg $
   if cfgPrintExit cfg == List
    then do
      _ <- note "Benchmarks:\n"
      mapM_ (note "  %s\n") (sort $ concatMap benchNames bs)
    else do
      case getLast $ cfgSummaryFile cfg of
        Just fn -> liftIO $ writeFile fn "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB\n"
        Nothing -> return ()
      env <- measureEnvironment
      let shouldRun b = null args || any (`isPrefixOf` b) args
      prep
      runAndAnalyse shouldRun env $ BenchGroup "" bs

-- | Display an error message from a command line parsing failure, and
-- exit.
parseError :: String -> IO a
parseError msg = do
  _ <- printError "Error: %s" msg
  _ <- printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

-- $bench
--
-- The 'Benchmarkable' typeclass represents the class of all code that
-- can be benchmarked.  Every instance must run a benchmark a given
-- number of times.  We are most interested in benchmarking two things:
--
-- * 'IO' actions.  Any 'IO' action can be benchmarked directly.
--
-- * Pure functions.  GHC optimises aggressively when compiling with
--   @-O@, so it is easy to write innocent-looking benchmark code that
--   doesn't measure the performance of a pure function at all.  We
--   work around this by benchmarking both a function and its final
--   argument together.

-- $io
--
-- Any 'IO' action can be benchmarked easily if its type resembles
-- this:
--
-- @
-- 'IO' a
-- @

-- $pure
--
-- Because GHC optimises aggressively when compiling with @-O@, it is
-- potentially easy to write innocent-looking benchmark code that will
-- only be evaluated once, for which all but the first iteration of
-- the timing loop will be timing the cost of doing nothing.
--
-- To work around this, we provide a special type, 'Pure', for
-- benchmarking pure code.  Values of this type are constructed using
-- one of two functions.
--
-- The first is a function which will cause results to be evaluated to
-- head normal form (NF):
--
-- @
-- 'nf' :: 'NFData' b => (a -> b) -> a -> 'Pure'
-- @
--
-- The second will cause results to be evaluated to weak head normal
-- form (the Haskell default):
--
-- @
-- 'whnf' :: (a -> b) -> a -> 'Pure'
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
--
-- The compiler will correctly infer that the number 1000 must have
-- the type 'Int', and the type of the expression is 'Pure'.

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
-- Because in this case the result will only be forced until it
-- reaches WHNF, what this would /actually/ benchmark is merely the
-- production of the first list element!
