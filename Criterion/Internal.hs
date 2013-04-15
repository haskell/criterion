{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Criterion
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
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

import Control.Monad (foldM, replicateM_, when, mplus)
import Control.Monad.Trans (liftIO)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as L
import Criterion.Analysis (Outliers(..), OutlierEffect(..), OutlierVariance(..),
                           SampleAnalysis(..), analyseSample,
                           classifyOutliers, noteOutliers)
import Criterion.Config (Config(..), Verbosity(..), fromLJ)
import Criterion.Environment (Environment(..))
import Criterion.IO (header, hGetResults)
import Criterion.IO.Printf (note, prolix, summary)
import Criterion.Measurement (getTime, runForAtLeast, secs, time_)
import Criterion.Monad (Criterion, getConfig, getConfigItem)
import Criterion.Report (Report(..), report)
import Criterion.Types (Benchmark(..), Benchmarkable(..),
                        Result(..), ResultForest, ResultTree(..))
import qualified Data.Vector.Unboxed as U
import Data.Monoid (getLast)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Types (Sample)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode(..), SeekMode(..), hClose, hSeek, openBinaryFile,
                  openBinaryTempFile)
import System.Mem (performGC)
import Text.Printf (printf)

-- | Run a single benchmark, and return timings measured when
-- executing it.
runBenchmark :: Benchmarkable b => Environment -> b -> Criterion Sample
runBenchmark env b = do
  _ <- liftIO $ runForAtLeast 0.1 10000 (`replicateM_` getTime)
  let minTime = envClockResolution env * 1000
  (testTime, testIters, _) <- liftIO $ runForAtLeast (min minTime 0.1) 1 (run b)
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
  -- Run the GC to make sure garabage created by previous benchmarks
  -- don't affect this benchmark.
  liftIO performGC
  times <- liftIO . fmap (U.map ((/ newItersD) . subtract (envClockCost env))) .
           U.replicateM sampleCount $ do
             when (fromLJ cfgPerformGC cfg) $ performGC
             time_ (run b newIters)
  return times

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Benchmarkable b => Environment -> String -> b
                 -> Criterion (Sample,SampleAnalysis,Outliers)
runAndAnalyseOne env _desc b = do
  times <- runBenchmark env b
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
  bs "mean" anMean
  summary ","
  bs "std dev" anStdDev
  summary "\n"
  vrb <- getConfigItem $ fromLJ cfgVerbosity
  let out = classifyOutliers times
  when (vrb == Verbose || (ovEffect > Unaffected && vrb > Quiet)) $ do
    noteOutliers out
    _ <- note "variance introduced by outliers: %.3f%%\n" (ovFraction * 100)
    _ <- note "variance is %s by outliers\n" wibble
    return ()
  return (times,an,out)
  where bs :: String -> Estimate -> Criterion ()
        bs d e = do _ <- note "%s: %s, lb %s, ub %s, ci %.3f\n" d
                      (secs $ estPoint e)
                      (secs $ estLowerBound e) (secs $ estUpperBound e)
                      (estConfidenceLevel e)
                    summary $ printf "%g,%g,%g"
                      (estPoint e)
                      (estLowerBound e) (estUpperBound e)


plotAll :: [Result] -> Criterion ()
plotAll descTimes = do
  report (zipWith (\n (Result d t a o) -> Report n d t a o) [0..] descTimes)

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
                           summary (show desc' ++ ",") -- String will be quoted
                           (x,an,out) <- runAndAnalyseOne env desc' b
                           let result = Single $ Result desc' x an out
                           liftIO $ L.hPut handle (encode result)
                           return $! k + 1
          | otherwise = return (k :: Int)
          where desc' = prefix pfx desc
      go !k (pfx, BenchGroup desc bs) =
          foldM go k [(prefix pfx desc, b) | b <- bs]
      go !k (pfx, BenchCompare bs) = do
                          l <- foldM go 0 [(pfx, b) | b <- bs]
                          let result = Compare l []
                          liftIO $ L.hPut handle (encode result)
                          return $! l + k
  _ <- go 0 ("", bs')

  rts <- (either fail return =<<) . liftIO $ do
    hSeek handle AbsoluteSeek 0
    rs <- hGetResults handle
    hClose handle
    case mbResultFile of
      Just _ -> return rs
      _      -> removeFile resultFile >> return rs

  mbCompareFile <- getConfigItem $ getLast . cfgCompareFile
  case mbCompareFile of
    Nothing -> return ()
    Just compareFile -> do
      liftIO $ writeFile compareFile $ resultForestToCSV rts

  let rs = flatten rts
  plotAll rs
  junit rs

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
        goQuickly pfx (BenchCompare bs) = mapM_ (goQuickly pfx) bs

        runOne b = do
            samples <- getConfigItem $ fromLJ cfgSamples
            liftIO $ run b samples

prefix :: String -> String -> String
prefix ""  desc = desc
prefix pfx desc = pfx ++ '/' : desc

flatten :: ResultForest -> [Result]
flatten [] = []
flatten (Single r    : rs) = r : flatten rs
flatten (Compare _ crs : rs) = flatten crs ++ flatten rs

resultForestToCSV :: ResultForest -> String
resultForestToCSV = unlines
                  . ("Reference,Name,% faster than reference" :)
                  . map (\(ref, n, p) -> printf "%s,%s,%.0f" ref n p)
                  . top
        where
          top :: ResultForest -> [(String, String, Double)]
          top [] = []
          top (Single _     : rts) = top rts
          top (Compare _ rts' : rts) = cmpRT rts' ++ top rts

          cmpRT :: ResultForest -> [(String, String, Double)]
          cmpRT [] = []
          cmpRT (Single r     : rts) = cmpWith r rts
          cmpRT (Compare _ rts' : rts) = case getReference rts' of
                                         Nothing -> cmpRT rts
                                         Just r  -> cmpRT rts' ++ cmpWith r rts

          cmpWith :: Result -> ResultForest -> [(String, String, Double)]
          cmpWith _   [] = []
          cmpWith ref (Single r     : rts) = cmp ref r : cmpWith ref rts
          cmpWith ref (Compare _ rts' : rts) = cmpRT rts'       ++
                                             cmpWith ref rts' ++
                                             cmpWith ref rts

          getReference :: ResultForest -> Maybe Result
          getReference []                   = Nothing
          getReference (Single r     : _)   = Just r
          getReference (Compare _ rts' : rts) = getReference rts' `mplus`
                                              getReference rts

cmp :: Result -> Result -> (String, String, Double)
cmp ref r = (description ref, description r, percentFaster)
    where
      percentFaster = (meanRef - meanR) / meanRef * 100

      meanRef = mean ref
      meanR   = mean r

      mean = estPoint . anMean . sampleAnalysis

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
    single r = printf "  <testcase name=\"%s\" time=\"%f\" />\n"
               (attrEsc $ description r) (estPoint $ anMean $ sampleAnalysis r)
    attrEsc = concatMap esc
      where
        esc '\'' = "&apos;"
        esc '"'  = "&quot;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '&'  = "&amp;"
        esc c    = [c]
