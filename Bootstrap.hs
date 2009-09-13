module Bootstrap where

import Control.Monad
import Control.Monad.ST
import Data.Array.Vector
import System.Random.Mersenne
import Data.Array.Vector.Algorithms.Intro (sort)
import Distribution.Normal hiding (mean)
import Math.Statistics.Fusion
import Statistics.Function (createU)

type Estimator = UArr Double -> Double

newtype Resample = Resample {
      fromResample :: UArr Double
    } deriving (Eq, Show)

resample :: MTGen -> Sample -> IO Resample
resample gen values = fmap Resample . createU n $ \_ -> do
    r <- random gen
    return (indexU values (abs r `mod` n))
  where n = lengthU values

type Sample = UArr Double

doResampling :: MTGen -> [Estimator] -> Int -> Sample
             -> IO [Resample]
doResampling gen ests numResamples samples = do
  results <- unsafeSTToIO . mapM (const (newMU numResamples)) $ ests
  loop 0 (zip ests results)
  unsafeSTToIO $ do
    mapM_ sort results
    mapM (fmap Resample . unsafeFreezeAllMU) results
 where
  loop k ers | k >= numResamples = return ()
             | otherwise = do
    r <- resample gen samples
    unsafeSTToIO . forM_ ers $ \(est,arr) ->
        writeMU arr k (est (fromResample r))
    loop (k+1) ers

data Estimate = Estimate {
      estPoint :: !Double
    , estLowerBound :: !Double
    , estUpperBound :: !Double
    , estConfidenceLevel :: !Double
    } deriving (Eq, Show)

data T = {-# UNPACK #-} !Double :< {-# UNPACK #-} !Double
infixl 2 :<

bootstrapBCA :: Double          -- ^ Confidence level.
             -> Sample          -- ^ Sample data.
             -> [Estimator]     -- ^ Estimators.
             -> [Resample]      -- ^ Resampled data.
             -> [Estimate]
bootstrapBCA confidenceLevel sample = zipWith e
  where
    e est (Resample resample) =
        Estimate {
            estPoint = pt
          , estLowerBound = indexU resample lo
          , estUpperBound = indexU resample hi
          , estConfidenceLevel = confidenceLevel
          }
      where
        pt    = est sample
        lo    = max (cumn a1) 0
          where a1 = bias + (b1 / (1 - accel * b1))
                b1 = bias + z1
        hi    = min (cumn a2) (ni - 1)
          where a2 = bias + (b2 / (1 - accel * b2))
                b2 = bias - z1
        z1    = inverse standard ((1 - confidenceLevel) / 2)
        cumn  = round . (*n) . cumulative standard
        bias  = inverse standard (probN / n)
          where probN = fromIntegral . lengthU . filterU (<pt) $ resample
        ni    = lengthU resample
        n     = fromIntegral ni
        accel = sumCubes / (6 * (sumSquares ** 1.5))
          where (sumSquares :< sumCubes) = foldlU f (0 :< 0) jack
                f (s :< c) j = s + d2 :< c + d2 * d
                    where d  = jackMean - j
                          d2 = d * d
                jackMean     = mean jack
        jack  = mapU f . enumFromToU 0 . subtract 1 . lengthU $ sample
          where f i = est (a `appendU` tailU b)
                  where (a,b) = splitAtU i sample
