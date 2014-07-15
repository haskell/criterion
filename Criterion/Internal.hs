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

import Control.Monad (foldM, when)
import Control.Monad.Trans (liftIO)
import Data.Binary (encode)
import Data.Int (Int64)
import Data.List (unfoldr)
import qualified Data.ByteString.Lazy as L
import Criterion.Analysis (Outliers(..), OutlierEffect(..), OutlierVariance(..),
                           SampleAnalysis(..), analyseSample,
                           classifyOutliers, noteOutliers)
import Criterion.Config (Config(..), Verbosity(..), fromLJ)
import Criterion.IO (header, hGetResults)
import Criterion.IO.Printf (note, prolix, writeCsv)
import Criterion.Measurement
import Criterion.Monad (Criterion, getConfig, getConfigItem)
import Criterion.Report (Report(..), report)
import Criterion.Types (Benchmark(..), Benchmarkable(..), Measured(..),
                        Payload(..), Result(..), measure, rescale)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Monoid (getLast)
import qualified Statistics.Matrix as M
import Statistics.Regression (ols, rSquare)
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
  cfg <- getConfig
  start <- liftIO $ performGC >> getTime
  let budget = 5
      loop [] _ = error "unpossible!"
      loop (iters:niters) acc = do
        when (fromLJ cfgPerformGC cfg) $ performGC
        startStats <- getGCStats
        startTime <- getTime
        startCpuTime <- getCPUTime
        startCycles <- getCycles
        run iters
        endTime <- getTime
        endCpuTime <- getCPUTime
        endCycles <- getCycles
        endStats <- getGCStats
        let m = applyGCStats endStats startStats $ measured {
                  measTime    = max 0 (endTime - startTime)
                , measCpuTime = max 0 (endCpuTime - startCpuTime)
                , measCycles  = max 0 (fromIntegral (endCycles - startCycles))
                , measIters   = iters
                }
        if endTime - start >= budget
          then return $! G.reverse (G.fromList acc)
          else loop niters (m:acc)
  liftIO $ loop (squish (unfoldr series 1)) []

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Maybe String -> Benchmarkable
                 -> Criterion (V.Vector Measured, SampleAnalysis, Outliers)
runAndAnalyseOne mdesc bm = do
  meas <- runBenchmark bm
  ci <- getConfigItem $ fromLJ cfgConfInterval
  numResamples <- getConfigItem $ fromLJ cfgResamples
  _ <- prolix "analysing with %d resamples\n" numResamples
  let times = measure (measTime . rescale) meas
  an@SampleAnalysis{..} <- liftIO $ analyseSample ci times numResamples
  let OutlierVariance{..} = anOutlierVar
  let wibble = case ovEffect of
                 Unaffected -> "unaffected" :: String
                 Slight -> "slightly inflated"
                 Moderate -> "moderately inflated"
                 Severe -> "severely inflated"
  regress meas
  (a,b,c) <- bs "mean   " anMean
  (d,e,f) <- bs "std dev" anStdDev
  case mdesc of
    Just desc -> writeCsv (desc,a,b,c,d,e,f)
    Nothing   -> writeCsv (a,b,c,d,e,f)
  vrb <- getConfigItem $ fromLJ cfgVerbosity
  let out = classifyOutliers times
  when (vrb == Verbose || (ovEffect > Slight && vrb > Quiet)) $ do
    when (vrb == Verbose) $ noteOutliers out
    _ <- note "variance introduced by outliers: %d%% (%s)\n"
         (round (ovFraction * 100) :: Int) wibble
    return ()
  _ <- note "\n"
  return (meas,an,out)
  where bs :: String -> Estimate -> Criterion (Double,Double,Double)
        bs d e = do
          _ <- note "%s   %s   (lb %s   ub %s   ci %.3f)\n" d
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
              -> Benchmark
              -> Criterion ()
runAndAnalyse p bs' = do
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
          | p desc'   = do _ <- note "benchmarking %s\n" desc'
                           (x,an,out) <- runAndAnalyseOne (Just desc') b
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

regress :: V.Vector Measured -> Criterion ()
regress meas = do
  let times = measure measTime meas
      iters = measure (fromIntegral . measIters) meas
      n     = G.length meas
      preds = M.fromVector n 1 iters
      coefs = ols preds times
      r2    = rSquare preds times coefs
  note "time      %s   (R\178 %.4g)\n" (secs (G.head coefs)) r2

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
