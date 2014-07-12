{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Criterion
-- Copyright   : (c) 2009-2013 Bryan O'Sullivan
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

import Control.Monad (foldM, replicateM_, when)
import Control.Monad.Trans (liftIO)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as L
import Criterion.Analysis (Outliers(..), OutlierEffect(..), OutlierVariance(..),
                           SampleAnalysis(..), analyseSample,
                           classifyOutliers, noteOutliers)
import Criterion.Config (Config(..), Verbosity(..), fromLJ)
import Criterion.Environment (Environment(..))
import Criterion.IO (header, hGetResults)
import Criterion.IO.Printf (note, prolix, writeCsv)
import Criterion.Measurement (getTime, runForAtLeast, secs,
                              time_)
import Criterion.Monad (Criterion, getConfig, getConfigItem)
import Criterion.Report (Report(..), report)
import Criterion.Types (Benchmark(..), Benchmarkable(..), Payload(..),
                        Result(..))
import qualified Data.Vector.Unboxed as U
import Data.Monoid (getLast)
import GHC.Stats (GCStats(..), getGCStats)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Types (Sample)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode(..), SeekMode(..), hClose, hSeek, openBinaryFile,
                  openBinaryTempFile)
import System.Mem (performGC)
import Text.Printf (printf)

-- | Run a single benchmark, and return timings measured when
-- executing it.
runBenchmark :: Environment -> Benchmarkable -> Criterion Sample
runBenchmark env (Benchmarkable run) = do
  _ <- liftIO $ runForAtLeast 0.1 10000 (`replicateM_` getTime)
  let minTime = envClockResolution env * 1000
  (testTime, testIters, _) <- liftIO $ runForAtLeast (min minTime 0.1) 1 run
  _ <- prolix "ran %d iterations in %s\n" testIters (secs testTime)
  cfg <- getConfig
  let newIters    = ceiling $ minTime * testItersD / testTime
      sampleCount = fromLJ cfgSamples cfg
      newItersD   = fromIntegral newIters
      testItersD  = fromIntegral testIters
      estTime     = (fromIntegral sampleCount * newItersD *
                     testTime / testItersD)
  when (fromLJ cfgVerbosity cfg > Normal || estTime > 5) $
    note "collecting %d samples, %d iterations each, in estimated %s\n"
       sampleCount newIters (secs estTime)
  -- Run the GC to make sure garbage created by previous benchmarks
  -- don't affect this benchmark.
  liftIO performGC
  times <- liftIO . fmap (U.map ((/ newItersD) . subtract (envClockCost env))) .
           U.replicateM sampleCount $ do
             when (fromLJ cfgPerformGC cfg) $ performGC
             gc0 <- getGCStats
             r <- time_ (run newIters)
             gc1 <- getGCStats
             print (bytesAllocated gc1 - bytesAllocated gc0)
             return r
  return times

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Environment -> Maybe String -> Benchmarkable
                 -> Criterion (Sample,SampleAnalysis,Outliers)
runAndAnalyseOne env mdesc bm = do
  times <- runBenchmark env bm
  ci <- getConfigItem $ fromLJ cfgConfInterval
  numResamples <- getConfigItem $ fromLJ cfgResamples
  _ <- prolix "analysing with %d resamples\n" numResamples
  an@SampleAnalysis{..} <- liftIO $ analyseSample ci times numResamples
  let OutlierVariance{..} = anOutlierVar
  let wibble = case ovEffect of
                 Unaffected -> "unaffected" :: String
                 Slight -> "slightly inflated"
                 Moderate -> "moderately inflated"
                 Severe -> "severely inflated"
  (a,b,c) <- bs "mean" anMean
  (d,e,f) <- bs "std dev" anStdDev
  case mdesc of
    Just desc -> writeCsv (desc,a,b,c,d,e,f)
    Nothing   -> writeCsv (a,b,c,d,e,f)
  vrb <- getConfigItem $ fromLJ cfgVerbosity
  let out = classifyOutliers times
  when (vrb == Verbose || (ovEffect > Unaffected && vrb > Quiet)) $ do
    noteOutliers out
    _ <- note "variance introduced by outliers: %.3f%%\n" (ovFraction * 100)
    _ <- note "variance is %s by outliers\n" wibble
    return ()
  return (times,an,out)
  where bs :: String -> Estimate -> Criterion (Double,Double,Double)
        bs d e = do
          _ <- note "%s: %s, lb %s, ub %s, ci %.3f\n" d
               (secs $ estPoint e)
               (secs $ estLowerBound e) (secs $ estUpperBound e)
               (estConfidenceLevel e)
          return (estPoint e, estLowerBound e, estUpperBound e)


plotAll :: [Result] -> Criterion ()
plotAll descTimes = do
  report (zipWith (\n (Single d (Payload t a o)) -> Report n d t a o) [0..] descTimes)

-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Environment
              -> Benchmark
              -> Criterion ()
runAndAnalyse p env bs' = do
  mbResultFile <- getConfigItem $ getLast . cfgResults
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
          | p desc'   = do _ <- note "\nbenchmarking %s\n" desc'
                           (x,an,out) <- runAndAnalyseOne env (Just desc') b
                           let result = Single desc' $ Payload x an out
                           liftIO $ L.hPut handle (encode result)
                           return $! k + 1
          | otherwise = return (k :: Int)
          where desc' = prefix pfx desc
      go !k (pfx, BenchGroup desc bs) =
          foldM go k [(prefix pfx desc, b) | b <- bs]
  _ <- go 0 ("", bs')

  rts <- (either fail return =<<) . liftIO $ do
    hSeek handle AbsoluteSeek 0
    rs <- hGetResults handle
    hClose handle
    case mbResultFile of
      Just _ -> return rs
      _      -> removeFile resultFile >> return rs

  plotAll rts
  junit rts

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

        runOne (Benchmarkable run) = do
            samples <- getConfigItem $ fromLJ cfgSamples
            liftIO $ run samples

prefix :: String -> String -> String
prefix ""  desc = desc
prefix pfx desc = pfx ++ '/' : desc

-- | Write summary JUnit file (if applicable)
junit :: [Result] -> Criterion ()
junit rs
  = do junitOpt <- getConfigItem (getLast . cfgJUnitFile)
       case junitOpt of
         Just fn -> liftIO $ writeFile fn msg
         Nothing -> return ()
  where
    msg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
          printf "<testsuite name=\"Criterion benchmarks\" tests=\"%d\">\n"
          (length rs) ++
          concatMap single rs ++
          "</testsuite>\n"
    single (Single d r) = printf "  <testcase name=\"%s\" time=\"%f\" />\n"
               (attrEsc d) (estPoint $ anMean $ sampleAnalysis r)
    attrEsc = concatMap esc
      where
        esc '\'' = "&apos;"
        esc '"'  = "&quot;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '&'  = "&amp;"
        esc c    = [c]
