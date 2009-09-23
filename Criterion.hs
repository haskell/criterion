{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, TypeOperators, GADTs #-}

module Criterion
    (
      Benchmarkable(..)
    , Benchmark
    , bench
    , sampleEnvironment
    , runBenchmark
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Parallel.Strategies
import Criterion.Config
import Data.Array.Vector
import Math.Statistics.Fusion
import Data.Int
import Data.Monoid
import Data.Time.Clock.POSIX
import Data.Typeable
import Text.Printf
import Prelude hiding (catch)
import Debug.Trace
import System.IO
import Criterion.Types
import Graphics.Rendering.Chart.Simple
import Statistics.Resampling.Bootstrap (Estimate(..), bootstrapBCA)
import Statistics.Resampling (resample)
import Statistics.Function (createIO, sort)
import Statistics.Types (Sample)
import Statistics.KernelDensity
import Statistics.RandomVariate
import Statistics.Quantile as Q (weightedAvg)
import qualified Statistics.Function as F
import System.IO.Unsafe
import System.Mem

data Environment = Environment {
      envClockResolution :: Double
    , envClockCost :: Double
    } deriving (Eq, Read, Show)

snd3 :: (a :*: b :*: c) -> b
snd3 (_ :*: b :*: _) = b

thd3 :: (a :*: b :*: c) -> c
thd3 (_ :*: _:*: c) = c

nullDev :: Handle
nullDev = unsafePerformIO $ openBinaryFile "/dev/null" WriteMode
{-# NOINLINE nullDev #-}

note :: (HPrintfType r) => Config -> String -> r
note cfg msg = if cfgVerbosity cfg > Quiet
               then hPrintf stdout msg
               else hPrintf nullDev msg

prolix :: (HPrintfType r) => Config -> String -> r
prolix cfg msg = if cfgVerbosity cfg == Verbose
                 then hPrintf stdout msg
                 else hPrintf nullDev msg

flush :: Config -> IO ()
flush prm = hFlush stdout

countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d

noteOutliers :: Config -> Outliers -> IO ()
noteOutliers prm o = do
  let frac n = (100::Double) * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int64 -> Double -> String -> IO ()
      check k t d = when (frac k > t) $
                    note prm "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    note prm "found %d outliers among %d samples (%.1g%%):\n"
             outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) 0 "low severe"
    check (lowMild o) 1 "low mild"
    check (highMild o) 1 "high mild"
    check (highSevere o) 0 "high severe"

analyseMean :: Config -> Sample -> Int -> IO Double
analyseMean prm a iters = do
  let m = mean a
  note prm "mean is %s (%d iterations)\n" (secs m) iters
  noteOutliers prm . classifyOutliers $ a
  return m
 where n = lengthU a

sampleEnvironment :: Config -> IO Environment
sampleEnvironment prm = do
  note prm "warming up\n"
  seed <- snd3 `fmap` runForAtLeast 0.1 10000 resolution
  note prm "estimating clock resolution...\n"
  clockRes <- thd3 `fmap` runForAtLeast 0.5 seed resolution >>=
              uncurry (analyseMean prm)
  note prm "estimating cost of a clock call...\n"
  clockCost <- cost (min (100000 * clockRes) 1) >>= uncurry (analyseMean prm)
  return $ Environment {
               envClockResolution = clockRes
             , envClockCost = clockCost
             }
  where
    resolution k = do
      times <- createIO (k+1) (const getTime)
      return (tailU . filterU (>=0) . zipWithU (-) (tailU times) $ times,
              lengthU times)
    cost timeLimit = do
      let timeClock k = time_ (replicateM_ k getTime)
      timeClock 1
      (_ :*: iters :*: elapsed) <- runForAtLeast 0.01 10000 timeClock
      times <- createIO (ceiling (timeLimit / elapsed)) $ \_ -> timeClock iters
      return (mapU (/ fromIntegral iters) times, lengthU times)



fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime
      
runBenchmark :: Config -> Environment -> Benchmark -> IO (Int, Sample)
runBenchmark prm env (Benchmark desc b) = do
  note prm "\nbenchmarking %s\n" desc
  runForAtLeast 0.1 10000 (`replicateM_` getTime)
  let minTime    = envClockResolution env * 1000
  (testTime :*: testIters :*: _) <- runForAtLeast (min minTime 0.1) 1 rpt
  prolix prm "ran %d iterations in %s\n" testIters (secs testTime)
  let newIters = ceiling $ minTime * fromIntegral testIters / testTime
      sampleCount = 100
  note prm "collecting %d samples, %d iterations each, in estimated %s\n"
       sampleCount newIters (secs (fromIntegral sampleCount * fromIntegral newIters * testTime / fromIntegral testIters))
  times <- mapU ((/ fromIntegral newIters) . subtract (envClockCost env)) `fmap`
           createIO sampleCount (\k -> time_ (rpt newIters))
  let totalIters = lengthU times * newIters
  analyseMean prm times totalIters
  return (totalIters, times)
  where
    rpt k | k <= 0    = return ()
          | otherwise = run b k >> rpt (k-1)

-- getStdGen >>= \g -> bchart g . bench "fib 20" $ \(k::Int) -> fib 20

data OutlierVariance = Unaffected
                     | Slight
                     | Moderate
                     | Severe
                       deriving (Eq, Ord, Show)

minBy :: (Ord b) => (a -> b) -> a -> a -> b
minBy f a b = min (f a) (f b)

outlierVariance :: Estimate     -- ^ Bootstrap estimate of sample mean.
                -> Estimate     -- ^ Bootstrap estimate of sample
                                --   standard deviation.
                -> Double       -- ^ Number of original iterations.
                -> (OutlierVariance, Double)
outlierVariance m sd a = (effect, varOutMin)
  where
    effect | varOutMin < 0.01 = Unaffected
           | varOutMin < 0.1  = Slight
           | varOutMin < 0.5  = Moderate
           | otherwise        = Severe
    varOutMin = (minBy varOut 1 (minBy cMax 0 µgMin)) / σb2
    varOut c  = (ac / a) * (σb2 - ac * σg2) where ac = a - c
    σb        = estPoint sd
    µa        = estPoint m / a
    µgMin     = µa / 2
    σg        = min (µgMin / 4) (σb / sqrt a)
    σg2       = σg * σg
    σb2       = σb * σb
    cMax x    = fromIntegral . floor $ -2 * k0 / (k1 + sqrt det)
      where
        k1    = σb2 - a * σg2 + ad
        k0    = -a * ad
        ad    = a * d
        d     = k * 2 where k = µa - x
        det   = k1 * k1 - 4 * σg2 * k0

sdInfo :: Estimate -> Estimate -> Double -> IO ()
sdInfo m sd a = do
  hPrintf stdout "variance introduced by outliers: %.3f%%\n" (v * 100)
  hPrintf stdout "variance is %s by outliers\n" wibble
  where
    wibble :: String
           = case effect of
               Unaffected -> "unaffected"
               Slight -> "slightly inflated"
               Moderate -> "moderately inflated"
               Severe -> "severely inflated"
    (effect, v) = outlierVariance m sd a

runAndAnalyse :: Config -> Environment -> Benchmark -> IO ()
runAndAnalyse cfg env b = do
  (totalIters, times) <- runBenchmark cfg env b
  let (points, pdf) = epanechnikovPDF 100 times
  writeFile "times.dat" (unlines . map show . fromU $ times)
  plotWindow [(0::Double)..] (fromU . sort $ times) ("run"::String) ("times"::String)
  plotWindow (fromU . fromPoints $ points) (fromU pdf) ("points"::String) ("pdf"::String)
  let ests = [mean,stddev]
      numResamples = fromLJ cfgResamples cfg
  note cfg "bootstrapping with %d resamples\n" numResamples
  res <- withSystemRandom (\gen -> resample gen ests numResamples times)
  let [em,es] = bootstrapBCA (fromLJ cfgConfInterval cfg) times ests res
  tell cfg "bootstrapped mean" em
  tell cfg "bootstrapped standard deviation" es
  sdInfo em es (fromIntegral $ totalIters)

tell :: Config -> String -> Estimate -> IO ()
tell cfg t e = note cfg "%s: %s, lb %s, ub %s, ci %.3f\n" t
               (secs $ estPoint e)
               (secs $ estLowerBound e) (secs $ estUpperBound e)
               (estConfidenceLevel e)
        
time :: IO a -> IO (Double :*: a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  return (end - start :*: result)

time_ :: IO a -> IO Double
time_ act = do
  start <- getTime
  result <- act
  end <- getTime
  return $! end - start

busyLoop :: Int64 -> Int64
busyLoop i | i <= 0 = i
           | otherwise = busyLoop (i-1)

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9 = printf "%.4g %s" t u
               | t >= 1e6 = printf "%.0f %s" t u
               | t >= 1e5 = printf "%.1f %s" t u
               | t >= 1e4 = printf "%.2f %s" t u
               | t >= 1e3 = printf "%.3f %s" t u
               | t >= 1e2 = printf "%.4f %s" t u
               | t >= 1e1 = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

data Outliers = Outliers {
      samplesSeen :: {-# UNPACK #-} !Int64
    , lowSevere   :: {-# UNPACK #-} !Int64
    -- ^ More than 3 times the IQR below the first quartile.
    , lowMild     :: {-# UNPACK #-} !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: {-# UNPACK #-} !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: {-# UNPACK #-} !Int64
    -- ^ More than 3 times the IQR above the third quartile.
    } deriving (Eq, Read, Show)

instance Monoid Outliers where
    mempty = Outliers 0 0 0 0 0
    mappend = addOutliers

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = foldlU ((. outlier) . mappend) mempty ssa
    where outlier e = Outliers {
                        samplesSeen = 1
                      , lowSevere = if e <= loS then 1 else 0
                      , lowMild = if e > loS && e <= loM then 1 else 0
                      , highMild = if e >= hiM && e < hiS then 1 else 0
                      , highSevere = if e >= hiS then 1 else 0
                      }
          loS = q1 - (iqr * 3)
          loM = q1 - (iqr * 1.5)
          hiM = q3 + (iqr * 1.5)
          hiS = q3 + (iqr * 3)
          q1  = Q.weightedAvg 1 4 ssa
          q3  = Q.weightedAvg 3 4 ssa
          ssa = sort sa
          iqr = q3 - q1

runForAtLeast :: Double -> Int -> (Int -> IO a) -> IO (Double :*: Int :*: a)
runForAtLeast howLong initSeed act = loop initSeed (0::Int) =<< getTime
  where
    loop !seed !iters initTime = do
      now <- getTime
      when (now - initTime > howLong * 10) $
        fail (printf "took too long to run: seed %d, iters %d" seed iters)
      elapsed :*: result <- time (act seed)
      if elapsed < howLong
        then loop (seed * 2) (iters+1) initTime
        else return (elapsed :*: seed :*: result)
