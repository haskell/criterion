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
      runAndAnalyse
    , runAndAnalyseOne
    , runNotAnalyse
    ) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, void, when)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import Data.Binary (encode)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L
import Criterion.Analysis (analyseSample, noteOutliers)
import Criterion.IO (header, hGetReports)
import Criterion.IO.Printf (note, printError, prolix, writeCsv)
import Criterion.Measurement (runBenchmark, secs)
import Criterion.Monad (Criterion)
import Criterion.Report (report)
import Criterion.Types hiding (measure)
import qualified Data.Map as Map
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode(..), SeekMode(..), hClose, hSeek, openBinaryFile,
                  openBinaryTempFile)
import Text.Printf (printf)

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Int -> String -> Benchmarkable -> Criterion Report
runAndAnalyseOne i desc bm = do
  Config{..} <- ask
  (meas,timeTaken) <- liftIO $ runBenchmark bm timeLimit
  when (timeTaken > timeLimit * 1.25) .
    void $ prolix "measurement took %s\n" (secs timeTaken)
  _ <- prolix "analysing with %d resamples\n" resamples
  erp <- runEitherT $ analyseSample i desc meas
  case erp of
    Left err -> printError "*** Error: %s\n" err
    Right rpt@Report{..} -> do
      let SampleAnalysis{..} = reportAnalysis
          OutlierVariance{..} = anOutlierVar
          wibble = case ovEffect of
                     Unaffected -> "unaffected" :: String
                     Slight -> "slightly inflated"
                     Moderate -> "moderately inflated"
                     Severe -> "severely inflated"
          (builtin, others) = splitAt 1 anRegress
      let r2 n = printf "%.3f R\178" n
      forM_ builtin $ \Regression{..} ->
        case Map.lookup "iters" regCoeffs of
          Nothing -> return ()
          Just t  -> bs secs "time" t >> bs r2 "" regRSquare
      bs secs "mean" anMean
      bs secs "std dev" anStdDev
      forM_ others $ \Regression{..} -> do
        _ <- bs r2 (regResponder ++ ":") regRSquare
        forM_ (Map.toList regCoeffs) $ \(prd,val) ->
          bs (printf "%.3g") ("  " ++ prd) val
      writeCsv (desc,
                estPoint anMean, estLowerBound anMean, estUpperBound anMean,
                estPoint anStdDev, estLowerBound anStdDev,
                estUpperBound anStdDev)
      when (verbosity == Verbose || (ovEffect > Slight && verbosity > Quiet)) $ do
        when (verbosity == Verbose) $ noteOutliers reportOutliers
        _ <- note "variance introduced by outliers: %d%% (%s)\n"
             (round (ovFraction * 100) :: Int) wibble
        return ()
      _ <- note "\n"
      return rpt
      where bs :: (Double -> String) -> String -> Estimate -> Criterion ()
            bs f metric Estimate{..} =
              note "%-20s %-10s (%s .. %s%s)\n" metric
                   (f estPoint) (f estLowerBound) (f estUpperBound)
                   (if estConfidenceLevel == 0.95 then ""
                    else printf ", ci %.3f" estConfidenceLevel)


-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Criterion ()
runAndAnalyse p bs' = do
  mbRawFile <- asks rawDataFile
  (rawFile, handle) <- liftIO $
    case mbRawFile of
      Nothing -> do
        tmpDir <- getTemporaryDirectory
        openBinaryTempFile tmpDir "criterion.dat"
      Just file -> do
        handle <- openBinaryFile file ReadWriteMode
        return (file, handle)
  liftIO $ L.hPut handle header

  let go !k (pfx, Environment mkenv mkbench) = do
        e <- liftIO $ do
               ee <- mkenv
               evaluate (rnf ee)
               return ee
        go k (pfx, mkbench e)
      go !k (pfx, Benchmark desc b)
          | p desc'   = do _ <- note "benchmarking %s\n" desc'
                           rpt <- runAndAnalyseOne k desc' b
                           liftIO $ L.hPut handle (encode rpt)
                           return $! k + 1
          | otherwise = return (k :: Int)
          where desc' = addPrefix pfx desc
      go !k (pfx, BenchGroup desc bs) =
          foldM go k [(addPrefix pfx desc, b) | b <- bs]
  _ <- go 0 ("", bs')

  rpts <- (either fail return =<<) . liftIO $ do
    hSeek handle AbsoluteSeek 0
    rs <- hGetReports handle
    hClose handle
    case mbRawFile of
      Just _ -> return rs
      _      -> removeFile rawFile >> return rs

  report rpts
  junit rpts

-- | Run a benchmark without analysing its performance.
runNotAnalyse :: Int64            -- ^ Number of loop iterations to run.
              -> (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Criterion ()
runNotAnalyse iters p bs' = goQuickly "" bs'
  where goQuickly :: String -> Benchmark -> Criterion ()
        goQuickly pfx (Environment mkenv mkbench) = do
            e <- liftIO mkenv
            goQuickly pfx (mkbench e)
        goQuickly pfx (Benchmark desc b)
            | p desc'   = do _ <- note "benchmarking %s\n" desc'
                             runOne b
            | otherwise = return ()
            where desc' = addPrefix pfx desc
        goQuickly pfx (BenchGroup desc bs) =
            mapM_ (goQuickly (addPrefix pfx desc)) bs

        runOne (Benchmarkable run) = liftIO (run iters)

-- | Write summary JUnit file (if applicable)
junit :: [Report] -> Criterion ()
junit rs
  = do junitOpt <- asks junitFile
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
