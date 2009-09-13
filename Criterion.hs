{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, TypeOperators, GADTs #-}

module Criterion
    (
     main
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Parallel.Strategies
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
import Graphics.Rendering.Chart.Simple
import Statistics.Resampling.Bootstrap (Estimate(..), bootstrapBCA)
import Statistics.Resampling (resample)
import System.Random.Mersenne
import Statistics.Function (createU)
import Statistics.Types (Sample)
import qualified Statistics.Function as F

data Config = Config {
      cfgConfidence :: Double
    } deriving (Eq, Read, Show, Typeable)

defaultConfig = Config {
                  cfgConfidence = 0.95
                }

data Params = Params {
      prmConfig :: Config
    , prmNull :: Handle
    } deriving (Typeable)

getParams :: IO Params
getParams = do
  null <- openFile "/dev/null" ReadWriteMode
  return $ Params {
               prmConfig = defaultConfig
             , prmNull = null
             }

class Benchmarkable b where
    run :: b -> Int -> IO ()

instance (NFData a) => Benchmarkable (Int -> a) where
    run f u = evaluate (f u) >> return ()

instance Benchmarkable (IO a) where
    run a _ = a >> return ()

data Environment = Environment {
      envClockResolution :: Double
    , envClockCost :: Double
    } deriving (Eq, Read, Show)

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

note :: (HPrintfType r) => Params -> String -> r
note prm msg = if True
               then hPrintf stdout msg
               else hPrintf (prmNull prm) msg

flush :: Params -> IO ()
flush prm = hFlush stdout

sampleClockResolution k = do
  times <- createU (k+1) (const getTime)
  return (lengthU times,
          tailU . filterU (>=0) . zipWithU (-) (tailU times) $ times)

sampleClockCost timeLimit = do
  let act k = fst `fmap` time (repeatTimes k getTime)
  act 10
  (_, seed, elapsed) <- runForAtLeast 0.01 10000 act
  times <- createU (ceiling (timeLimit / elapsed)) (const (act seed))
  return (lengthU times * seed, mapU (/ fromIntegral seed) times)

anyOutliers :: Outliers -> Bool
anyOutliers (Outliers _ a b c d) = a > 0 || b > 0 || c > 0 || d > 0

countOutliers (Outliers _ a b c d) = a + b + c + d

noteOutliers :: Params -> Outliers -> IO ()
noteOutliers prm o = do
  let frac :: Int -> Double
      frac n = 100 * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int -> String -> IO ()
      check k d = when (frac k > 0) $
                  note prm "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    note prm "found %d outliers among %d samples (%.1g%%):\n"
             outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) "low severe"
    check (lowMild o) "low mild"
    check (highMild o) "high mild"
    check (highSevere o) "high severe"

analyseMean :: Params -> UArr Double -> Int -> IO Double
analyseMean prm a iters = do
  let m = mean a
  note prm "mean is %s (%d iterations, %d samples)\n" (secs m) iters (lengthU a)
  noteOutliers prm . classifyOutliers . sort $ a
  return m

sampleEnvironment :: Params -> IO Environment
sampleEnvironment prm = do
  note prm "warming up\n"
  let settleTime = 1
  seed <- snd3 `fmap` runForAtLeast 0.25 10000 sampleClockResolution
  note prm "estimating clock resolution..." >> flush prm
  (rcount, resSamples) <- thd3 `fmap` runForAtLeast settleTime seed sampleClockResolution
  clockRes <- analyseMean prm resSamples rcount
  note prm "estimating clock cost..." >> flush prm
  (ccount, costSamples) <- sampleClockCost (max (100000 * clockRes) settleTime)
  clockCost <- analyseMean prm costSamples ccount
  return $ Environment {
               envClockResolution = clockRes
             , envClockCost = clockCost
             }

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime
      
repeatTimes :: Int -> IO a -> IO ()
repeatTimes k = sequence_ . take k . repeat

runBenchmark :: Params -> Environment -> Benchmark -> IO (Int, Sample)
runBenchmark prm env (Benchmark desc b) = do
  note prm "\nbenchmarking %s\n" desc
  runForAtLeast 0.1 10000 (\k -> repeatTimes k getTime)
  let runTime    = envClockResolution env * 100000
      settleTime = 0.01
  (elapsed, seed, _) <- runForAtLeast settleTime 1 (rpt 0)
  let ratio = settleTime / elapsed
      newSeed = ceiling . toRational $ fromIntegral seed * (settleTime / elapsed)
      niters = ceiling . toRational $ runTime / settleTime
  times <- mapU ((/ fromIntegral newSeed) . subtract (envClockCost env)) `fmap`
           createU niters (\k -> fmap fst . time . rpt k $ newSeed)
  analyseMean prm times (lengthU times * newSeed)
  return (lengthU times * newSeed, times)
  where
    rpt n k | k <= 0    = return ()
            | otherwise = run b k >> rpt n (k-1)

simplePDF :: (Double -> Double)
          -> (Double -> Double -> Double -> Double -> Double)
          -> Sample
          -> (Points, UArr Double)
simplePDF fbw fpdf sample = (points, estimatePDF fpdf bw sample points)
    where points = choosePDFPoints numPoints bw sample
          bw = bandwidth fbw sample
          numPoints = min (lengthU sample) 100

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sdInfo :: Estimate -> Estimate -> Double -> IO ()
sdInfo m sd a = do
  hPrintf stdout "variance introduced by outliers: %.3f%%\n" (varOutMin * 100)
  hPrintf stdout "variance is %s by outliers\n" wibble
  where
    wibble :: String
      | varOutMin < 0.01 = "unaffected"
      | varOutMin < 0.1 = "somewhat inflated"
      | varOutMin < 0.5 = "inflated"
      | otherwise = "severely inflated"
    varOutMin = min (varOutliers 1)
                    (varOutliers (min (cMax 0) (cMax muGMin))) / sB2
    varOutliers c = (ac / a) * (sB2 - ac * sG2) where ac = a - c
    sigmaB = estPoint sd
    muA = estPoint m / a
    muGMin = muA / 2
    sigmaG = min (muGMin / 4) (sigmaB / sqrt a)
    sG2 = sigmaG * sigmaG
    sB2 = sigmaB * sigmaB
    cMax x = fromIntegral . floor $ -2 * k0 / (k1 + sqrt det)
      where
        k1 = sB2 - a * sG2 + a * maMX2
        k0 = -a * a * maMX2
        maMX2 = k * 2 where k = muA - x
        det = k1 * k1 - 4 * sG2 * k0

bchart :: MTGen -> Benchmark -> IO ()
bchart gen b = do
  prm <- getParams
  env <- sampleEnvironment prm
  (actions, times) <- runBenchmark prm env b
  let (points, pdf) = simplePDF epanechnikovBW epanechnikovPDF times
  writeFile "times.dat" (unlines . map show . fromU $ times)
  plotWindow [(0::Double)..] (fromU . unsort . sort $ times) ("run"::String) ("times"::String)
  plotWindow (fromU . fromPoints $ points) (fromU pdf) ("points"::String) ("pdf"::String)
  let ests = [mean,stddev]
  res <- resample gen ests 10 times
  let [em,es] = bootstrapBCA 0.95 times ests res
  tell "mean" em
  tell "stddev" es
  sdInfo em es (fromIntegral $ actions)

tell :: String -> Estimate -> IO ()
tell t e = hPrintf stdout "%s: %s, lb %s, ub %s, ci %.3f\n" t
           (secs $ estPoint e)
           (secs $ estLowerBound e) (secs $ estUpperBound e)
           (estConfidenceLevel e)
        
benchmark bs = do
  prm <- getParams
  env <- sampleEnvironment prm
  mapM_ (runBenchmark prm env) bs

time :: IO a -> IO (Double, a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  return (end - start, result)

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

data Benchmark where
    Benchmark :: Benchmarkable b => String -> b -> Benchmark

bench :: Benchmarkable b => String -> b -> Benchmark
bench = Benchmark

instance Show Benchmark where
    show (Benchmark d _) = "Benchmark " ++ show d

main = benchmark [bench "sleep 0.1" $ threadDelay 100000,
                  bench "return nothing" $ \(k::Int) -> (1::Int),
                  bench "empty putStr" $ putStr ""]

newtype Sorted a = Sorted { unsort :: a }
    deriving (Eq, Ord, Show)

instance Functor Sorted where
    fmap f (Sorted a) = Sorted (f a)

sort :: (UA a, Ord a) => UArr a -> Sorted (UArr a)
sort = Sorted . F.sort

isSorted :: (UA a, Ord a) => UArr a -> Bool
isSorted a = sort a == Sorted a

-- | Two divided by the square root of pi.
m_2_sqrt_pi = 2 / sqrt pi

-- | One divided by the square root of two.
m_sqrt_1_2 = 1 / sqrt 2

-- | Bandwidth estimator for an Epanechnikov kernel.
epanechnikovBW :: Double -> Double
epanechnikovBW n = (80 / (n * m_2_sqrt_pi)) ** 0.2

-- | Bandwidth estimator for a Gaussian kernel.
gaussianBW :: Double -> Double
gaussianBW n = (4 / (n * 3)) ** 0.2

-- | Compute the optimal bandwidth from the observed data for the given
-- kernel.
bandwidth :: (Double -> Double)
          -> Sample
          -> Double
bandwidth k values = stddev values * k (fromIntegral $ lengthU values)

newtype Points = Points { fromPoints :: UArr Double }

-- | Choose a uniform range of points at which to estimate a sample's
-- probability density function.
--
-- If you are using a Gaussian kernel, multiply the sample's bandwidth
-- by 3 before passing it to this function.
choosePDFPoints :: Int             -- ^ Number of points to select
                -> Double          -- ^ Sample bandwidth
                -> Sample          -- ^ Input data
                -> Points
choosePDFPoints n h sample = Points . mapU f $ enumFromToU 0 n'
  where lo  = minimumU sample - h
        hi  = maximumU sample + h
        d   = (hi - lo) / fromIntegral n'
        f i = lo + fromIntegral i * d
        n'  = n - 1

-- | Epanechnikov kernel for probability density function estimation.
epanechnikovPDF :: Double       -- ^ Scaling factor (1\//nh/)
                -> Double       -- ^ Bandwidth (/h/)
                -> Double       -- ^ Point at which to sample input (/p/)
                -> Double       -- ^ Sample value (/v/)
                -> Double
epanechnikovPDF f h p v
    | abs u <= 1 = f * (1 - u * u)
    | otherwise  = 0
    where u = (v - p) / (h * 0.75)

gaussianPDF f h p v = exp (-0.5 * u * u) * g
    where u = (v - p) / h
          g = f * m_2_sqrt_pi * m_sqrt_1_2

-- | Kernel density estimator, providing a non-parametric way of
-- estimating the probability density function (or /PDF/) of a random
-- variable.
estimatePDF :: (Double -> Double -> Double -> Double -> Double)
            -- ^ Kernel function
            -> Double           -- ^ Bandwidth
            -> Sample           -- ^ Sample data
            -> Points           -- ^ Points in sample range at which to estimate
            -> UArr Double
estimatePDF kernel h sample = mapU k . fromPoints
  where
    k p = sumU . mapU (kernel f h p) $ sample
    f   = 1 / (h * fromIntegral (lengthU sample))

-- | Use the weighted average method to estimate the @k@th
-- @q@-quantile of a set of samples.
quantile :: Int -> Int -> Sorted Sample -> Double
quantile k q (Sorted a) =
    assert (q >= 2) .
    assert (k >= 0) .
    assert (k < q) .
    assert (allU (not . isNaN) a) $
    aj + g * (aj1 - aj)
  where
    j   = floor idx
    idx = fromIntegral (lengthU a - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    aj  = indexU a j
    aj1 = indexU a (j+1)

data Outliers = Outliers {
      samplesSeen :: !Int
    , lowSevere :: !Int
    , lowMild :: !Int
    , highMild :: !Int
    , highSevere :: !Int
    } deriving (Eq, Read, Show)

instance Monoid Outliers where
    mempty = Outliers 0 0 0 0 0
    mappend = addOutliers

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sorted Sample -> Outliers
classifyOutliers sa = foldlU ((. outlier) . mappend) mempty (unsort sa)
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
          q1  = quantile 1 4 sa
          q3  = quantile 3 4 sa
          iqr = q3 - q1

autocovariance :: Sample -> UArr Double
autocovariance a = mapU f . enumFromToU 0 $ l-2
  where f k = sumU (zipWithU (*) (takeU (l-k) c) (sliceU c k (l-k)))
              / fromIntegral l
        c   = mapU (subtract (mean a)) a
        l   = lengthU a

-- | Given a data set, compute its autocorrelation function, and the upper
-- and lower bounds of confidence intervals for each element.
autocorrelation :: Sample -> (UArr Double, UArr Double, UArr Double)
autocorrelation a = (r, ci (-), ci (+))
  where r           = mapU (/ headU c) c
          where c   = autocovariance a
        dllse       = mapU f . scanl1U (+) . mapU (join (*)) $ r
          where f v = 1.96 * sqrt ((v * 2 + 1) / l)
        l           = fromIntegral (lengthU a)
        ci f        = consU 1 . tailU . mapU (f (-1/l)) $ dllse

runForAtLeast :: Double -> Int -> (Int -> IO a) -> IO (Double, Int, a)
runForAtLeast howLong initSeed act = loop initSeed (0::Int) =<< getTime
  where
    loop !seed !iters initTime = do
      now <- getTime
      when (now - initTime > howLong * 10) $
        fail (printf "took too long to run: seed %d, iters %d" seed iters)
      (elapsed, result) <- time (act seed)
      if elapsed < howLong
        then loop (seed * 2) (iters+1) initTime
        else return (elapsed, seed, result)

allSane :: UArr Double -> Bool
allSane = allU sane
    where sane e = not (isNaN e || isInfinite e)

repeatFor :: Double -> IO a -> IO (Double, [a])
repeatFor howlong act = loop [] =<< getTime
  where
    loop acc initTime = do
      now <- getTime
      if now - initTime > howlong
        then return (now - initTime, acc)
        else do
          a <- act
          loop (a:acc) initTime

nums :: UArr Double
nums = toU [47, 64, 23, 71, 38, 64, 55, 41, 59, 48]

n70 :: UArr Double
n70 = toU [ 47, 64, 23, 71, 38, 64, 55, 41, 59, 48,
            71, 35, 57, 40, 58, 44, 80, 55, 37, 74,
            51, 57, 50, 60, 45, 57, 50, 45, 25, 59,
            50, 71, 56, 74, 50, 58, 45, 54, 36, 54,
            48, 55, 45, 57, 50, 62, 44, 64, 43, 52,
            38, 59, 55, 41, 53, 49, 34, 35, 54, 45,
            68, 38, 50, 60, 39, 59, 40, 57, 54, 23 ]

r15 :: UArr Double
r15 = toU [ 1.0,
            -- BOX-JENKINS does not have this lag 0 result in Table
            -- 2.2 on p. 34, but you need it in order to compare with
            -- r below
            -0.39, 0.30, -0.17, 0.07, -0.10,
            -0.05, 0.04, -0.04, -0.01, 0.01,
            0.11, -0.07, 0.15, 0.04, -0.01 ]
