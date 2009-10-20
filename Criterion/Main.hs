-- |
-- Module      : Criterion.Main
-- Copyright   : (c) Bryan O'Sullivan 2009
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
    -- * Benchmarking pure code
    -- $eval

    -- ** Let-floating
    -- $letfloat

    -- ** Worker-wrapper transformation
    -- $worker

    -- * Types
      Benchmarkable(..)
    , Benchmark
    -- * Constructing benchmarks
    , bench
    , bgroup
    -- * Running benchmarks
    , defaultMain
    , defaultMainWith
    -- * Other useful code
    , defaultOptions
    , parseArgs
    ) where

import Control.Monad (MonadPlus(..))
import Criterion (runAndAnalyse)
import Criterion.Config
import Criterion.Environment (measureEnvironment)
import Criterion.IO (note, printError)
import Criterion.MultiMap (singleton)
import Criterion.Types (Benchmarkable(..), Benchmark(..), bench, benchNames, bgroup)
import Data.List (isPrefixOf, sort)
import Data.Monoid (Monoid(..), Last(..))
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import Text.ParserCombinators.Parsec

-- | Parse a plot output.
parsePlot :: Parser PlotOutput
parsePlot = try (dim "window" Window 800 600)
    `mplus` try (dim "win" Window 800 600)
    `mplus` try (dim "pdf" PDF 432 324)
    `mplus` try (dim "png" PNG 800 600)
    `mplus` try (dim "svg" SVG 432 324)
    `mplus` (string "csv" >> return CSV)
  where dim s c dx dy = do
          string s
          try (uncurry c `fmap` dimensions) `mplus`
              (eof >> return (c dx dy))
        dimensions = do
            char ':'
            a <- many1 digit
            char 'x'
            b <- many1 digit
            case (reads a, reads b) of
              ([(x,[])],[(y,[])]) -> return (x, y)
              _                   -> mzero
           <?> "dimensions"

-- | Parse a plot type.
plot :: Plot -> String -> IO Config
plot p s = case parse parsePlot "" s of
             Left _err -> parseError "unknown plot type\n"
             Right t   -> return mempty { cfgPlot = singleton p t }

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
          "print a list of all benchmark names, then exit"
 , Option ['k'] ["plot-kde"] (ReqArg (plot KernelDensity) "TYPE")
          "plot kernel density estimate of probabilities"
 , Option [] ["kde-same-axis"] (noArg mempty {cfgPlotSameAxis = ljust True })
          "plot all KDE graphs with the same X axis range (useful for comparison)"
 , Option ['q'] ["quiet"] (noArg mempty { cfgVerbosity = ljust Quiet })
          "print less output"
 , Option [] ["resamples"]
          (ReqArg (pos "resample count"$ \n -> mempty { cfgResamples = n }) "N")
          "number of bootstrap resamples to perform"
 , Option ['s'] ["samples"]
          (ReqArg (pos "sample count" $ \n -> mempty { cfgSamples = n }) "N")
          "number of samples to collect"
 , Option ['t'] ["plot-timing"] (ReqArg (plot Timing) "TYPE")
          "plot timings"
 , Option ['V'] ["version"] (noArg mempty { cfgPrintExit = Version })
          "display version, then exit"
 , Option ['v'] ["verbose"] (noArg mempty { cfgVerbosity = ljust Verbose })
          "print more output"
 ]

printBanner :: Config -> IO ()
printBanner cfg =
    case cfgBanner cfg of
      Last (Just b) -> note cfg "%s\n" b
      _             -> note cfg "Hey, nobody told me what version I am!\n"

printUsage :: [OptDescr (IO Config)] -> ExitCode -> IO a
printUsage options exitCode = do
  p <- getProgName
  putStr (usageInfo ("Usage: " ++ p ++ " [OPTIONS] [BENCHMARKS]") options)
  mapM_ putStrLn [
       ""
    , "If no benchmark names are given, all are run"
    , "Otherwise, benchmarks are run by prefix match"
    , ""
    , "Plot types:"
    , "  window or win   display a window immediately"
    , "  csv             save a CSV file"
    , "  pdf             save a PDF file"
    , "  png             save a PNG file"
    , "  svg             save an SVG file"
    , ""
    , "You can specify plot dimensions via a suffix, e.g. \"window:640x480\""
    , "Units are pixels for png and window, 72dpi points for pdf and svg"
    ]
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
-- >        bgroup "fib" [ bench "fib 10" $ \n -> fib (10+n-n))
-- >                     , bench "fib 35" $ \n -> fib (35+n-n))
-- >                     , bench "fib 37" $ \n -> fib (37+n-n))
-- >                     ]
-- >                    ]
defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
--
-- Example:
--
-- > import Criterion.Config
-- > import qualified Criterion.MultiMap as M
-- >
-- > myConfig = defaultConfig {
-- >              -- Always display an 800x600 window with curves.
-- >              cfgPlot = M.singleton KernelDensity (Window 800 600)
-- >            }
-- > 
-- > main = defaultMainWith myConfig [
-- >          bench "fib 30" $ \(n::Int) -> fib (30+n-n)
-- >        ]
--
-- If you save the above example as @\"Fib.hs\"@, you should be able
-- to compile it as follows:
--
-- > ghc -O --make Fib
--
-- Run @\"Fib --help\"@ on the command line to get a list of command
-- line options.
defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith defCfg bs = do
  (cfg, args) <- parseArgs defCfg defaultOptions =<< getArgs
  if cfgPrintExit cfg == List
    then do
      note cfg "Benchmarks:\n"
      mapM_ (note cfg "  %s\n") (sort $ concatMap benchNames bs)
    else do
      env <- measureEnvironment cfg
      let shouldRun b = null args || any (`isPrefixOf` b) args
      runAndAnalyse shouldRun cfg env $ BenchGroup "" bs

-- | Display an error message from a command line parsing failure, and
-- exit.
parseError :: String -> IO a
parseError msg = do
  printError "Error: %s" msg
  printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

-- $eval
--
-- Because GHC optimises aggressively when compiling with @-O@, it is
-- easy to write innocent-looking benchmark code that will only be
-- evaluated once, for which all but the first iteration of the timing
-- loop will be timing the cost of doing nothing.
--
-- The 'Int' parameter that is passed into your benchmark function is
-- important: you'll almost certainly need to use it somehow in order
-- to ensure that your code will not get optimised away.

-- $letfloat
--
-- The following is an example of innocent-looking code that will not
-- benchmark correctly:
--
-- > b = bench "fib 10" $ \(_::Int) -> fib 10
--
-- GHC will notice that the body is constant, and use let-floating to
-- transform the function into a form more like this:
--
-- > lvl = fib 10
-- > b = bench "fib 10" $ \(::_Int) -> lvl
--
-- Here, it is obvious that the CAF @lvl@ only needs to be evaluated
-- once, and this is indeed what happens.  The first iteration in the
-- timing loop will measure a realistic time. All other iterations
-- will take a few dozen nanoseconds, since the original thunk for
-- @lvl@ has already been overwritten with the result of its first
-- evaluation.
--
-- One somewhat unreliable way to defeat let-floating is to disable it:
--
-- > {-# OPTIONS_GHC -fno-full-laziness #-}
--
-- If you are trying to benchmark an inlined function, turning off the
-- let-floating transformation may end up causing slower code to be
-- generated.
--
-- A much more reliable way to defeat let-floating is to find a way to
-- make use of the 'Int' that the benchmarking code passes in.
--
-- > bench "fib 10" $ \n -> fib (10+n-n)
--
-- GHC is not yet smart enough to see that adding and subtracting @n@
-- amounts to a no-op. This trick is enough to convince it not to
-- let-float the function's body out, since the body is no longer
-- constant.

-- $worker
--
-- Another GHC optimisation is worker-wrapper transformation.  Suppose
-- you want to time insertion of key\/value pairs into a map.  You
-- might perform the insertion via a (/strict/!) fold:
--
-- > import qualified Data.IntMap as I
-- > import Data.List (foldl')
-- >
-- > intmap :: Int -> I.IntMap Int
-- > intmap n = foldl' (\m k -> I.insert k k m) I.empty [0..n]
-- >
-- > b = bench "intmap 10k" $ \(_::Int) -> intmap 10000
--
-- Compile this /without/ @-fno-full-laziness@, and the body of the
-- anonymous function we're benchmarking gets let-floated out to the
-- top level.
--
-- > lvl = intmap 10000
-- > b = bench "intmap 10k" $ \(_::Int) -> lvl
--
-- Compile it /with/ @-fno-full-laziness@, and let-floating occurs
-- /anyway/, this time due to GHC's worker-wrapper transformation.
--
-- Once again, the response is to use the parameter that the
-- benchmarking code passes in.
--
-- > intmap :: Int -> Int -> I.IntMap Int
-- > intmap n i = foldl' (\m k -> I.insert k k m) I.empty [0..n+i-i]
-- >
-- > b = bench "intmap 10k" $ intmap 10000
