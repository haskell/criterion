{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Criterion
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core benchmarking code.

module Criterion.Internal
    (
      runBenchmark
    , runAndAnalyse
    , runNotAnalyse
    , prefix
    ) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Trans (liftIO)
import Data.Binary (encode)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified Data.ByteString.Lazy as L
import Criterion.Analysis (analyseSample, noteOutliers)
import Criterion.IO (header, hGetReports)
import Criterion.IO.Printf (note, prolix, writeCsv)
import Criterion.Measurement
import Criterion.Monad (Criterion, getConfig, getConfigItem)
import Criterion.Report (report)
import Criterion.Types
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Map as Map
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode(..), SeekMode(..), hClose, hSeek, openBinaryFile,
                  openBinaryTempFile)
import System.Mem (performGC)
import Text.Printf (printf)

-- Our series starts its growth very slowly when we begin at 1, so we
-- eliminate repeated values.
squish :: (Eq a) => [a] -> [a]
squish ys = foldr go [] ys
  where go x xs = x : dropWhile (==x) xs

series :: Double -> Maybe (Int64, Double)
series k = Just (truncate l, l)
  where l = k * 1.05

-- | Run a single benchmark, and return measurements collected while
-- executing it.
runBenchmark :: Benchmarkable -> Criterion (V.Vector Measured)
runBenchmark (Benchmarkable run) = do
  liftIO $ run 1
  Config{..} <- getConfig
  start <- liftIO $ performGC >> getTime
  let budget = 5
      loop [] _ = error "unpossible!"
      loop (iters:niters) acc = do
        when forceGC $ performGC
        startStats <- getGCStats
        startTime <- getTime
        startCycles <- getCycles
        run iters
        endTime <- getTime
        endCycles <- getCycles
        endStats <- getGCStats
        let m = applyGCStats endStats startStats $ measured {
                  measTime   = max 0 (endTime - startTime)
                , measCycles = max 0 (fromIntegral (endCycles - startCycles))
                , measIters  = iters
                }
        if endTime - start >= budget
          then return $! G.reverse (G.fromList acc)
          else loop niters (m:acc)
  liftIO $ loop (squish (unfoldr series 1)) []

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Int -> String -> Benchmarkable -> Criterion Report
runAndAnalyseOne i desc bm = do
  meas <- runBenchmark bm
  Config{..} <- getConfig
  _ <- prolix "analysing with %d resamples\n" resamples
  rpt@Report{..} <- liftIO $ analyseSample i desc confInterval meas resamples
  let SampleAnalysis{..} = reportAnalysis
      OutlierVariance{..} = anOutlierVar
  let wibble = case ovEffect of
                 Unaffected -> "unaffected" :: String
                 Slight -> "slightly inflated"
                 Moderate -> "moderately inflated"
                 Severe -> "severely inflated"
  forM_ anRegress $ \Regression{..} ->
    case Map.lookup "time" regCoeffs of
      Nothing -> return ()
      Just t  -> note "%-8s  %s   (R\178 %.4g)\n"
                      "time" (secs t) regRSquare
  (a,b,c) <- bs "mean   " anMean
  (d,e,f) <- bs "std dev" anStdDev
  writeCsv (desc,a,b,c,d,e,f)
  when (verbosity == Verbose || (ovEffect > Slight && verbosity > Quiet)) $ do
    when (verbosity == Verbose) $ noteOutliers reportOutliers
    _ <- note "variance introduced by outliers: %d%% (%s)\n"
         (round (ovFraction * 100) :: Int) wibble
    return ()
  _ <- note "\n"
  return rpt
  where bs :: String -> Estimate -> Criterion (Double,Double,Double)
        bs d e = do
          _ <- note "%s   %s   (lb %s   ub %s   ci %.3f)\n" d
               (secs $ estPoint e)
               (secs $ estLowerBound e) (secs $ estUpperBound e)
               (estConfidenceLevel e)
          return (estPoint e, estLowerBound e, estUpperBound e)


-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Criterion ()
runAndAnalyse p bs' = do
  mbResultFile <- getConfigItem reportFile
  (resultFile, handle) <- liftIO $
    case mbResultFile of
      Nothing -> do
        tmpDir <- getTemporaryDirectory
        openBinaryTempFile tmpDir "criterion.dat"
      Just file -> do
        handle <- openBinaryFile file ReadWriteMode
        return (file, handle)
  liftIO $ L.hPut handle header

  let go !k (pfx, Benchmark desc b)
          | p desc'   = do _ <- note "benchmarking %s\n" desc'
                           rpt <- runAndAnalyseOne k desc' b
                           liftIO $ L.hPut handle (encode rpt)
                           return $! k + 1
          | otherwise = return (k :: Int)
          where desc' = prefix pfx desc
      go !k (pfx, BenchGroup desc bs) =
          foldM go k [(prefix pfx desc, b) | b <- bs]
  _ <- go 0 ("", bs')

  rpts <- (either fail return =<<) . liftIO $ do
    hSeek handle AbsoluteSeek 0
    rs <- hGetReports handle
    hClose handle
    case mbResultFile of
      Just _ -> return rs
      _      -> removeFile resultFile >> return rs

  report rpts
  junit rpts

runNotAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Criterion ()
runNotAnalyse p bs' = goQuickly "" bs'
  where goQuickly :: String -> Benchmark -> Criterion ()
        goQuickly pfx (Benchmark desc b)
            | p desc'   = do _ <- note "benchmarking %s\n" desc'
                             runOne b
            | otherwise = return ()
            where desc' = prefix pfx desc
        goQuickly pfx (BenchGroup desc bs) =
            mapM_ (goQuickly (prefix pfx desc)) bs

        runOne (Benchmarkable run) = liftIO (run 1)

prefix :: String -> String -> String
prefix ""  desc = desc
prefix pfx desc = pfx ++ '/' : desc

-- | Write summary JUnit file (if applicable)
junit :: [Report] -> Criterion ()
junit rs
  = do junitOpt <- getConfigItem junitFile
       case junitOpt of
         Just fn -> liftIO $ writeFile fn msg
         Nothing -> return ()
  where
    msg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
          printf "<testsuite name=\"Criterion benchmarks\" tests=\"%d\">\n"
          (length rs) ++
          concatMap single rs ++
          "</testsuite>\n"
    single Report{..} = printf "  <testcase name=\"%s\" time=\"%f\" />\n"
               (attrEsc reportName) (estPoint $ anMean $ reportAnalysis)
    attrEsc = concatMap esc
      where
        esc '\'' = "&apos;"
        esc '"'  = "&quot;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '&'  = "&amp;"
        esc c    = [c]
