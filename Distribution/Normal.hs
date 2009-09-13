module Distribution.Normal where

import Control.Exception (assert)
import Data.Array.Vector
import qualified Distribution as D
import qualified Math.Statistics.Fusion as S
import SpecialMath

data NormalDistribution = NormalDistribution {
      mean     :: {-# UNPACK #-} !Double
    , variance :: {-# UNPACK #-} !Double
    , pdfDenom :: {-# UNPACK #-} !Double
    , cdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Ord, Read, Show)

instance D.Distribution NormalDistribution where
    probability = probability
    cumulative  = cumulative
    inverse     = inverse

sqrt_2 :: Double
sqrt_2 = sqrt 2

sqrt_2_pi :: Double
sqrt_2_pi = sqrt (2 * pi)

standard :: NormalDistribution
standard = NormalDistribution {
             mean = 0.0
           , variance = 1.0
           , cdfDenom = sqrt_2
           , pdfDenom = sqrt_2_pi
           }

fromParams :: Double -> Double -> NormalDistribution
fromParams m v = assert (v > 0) $
                 NormalDistribution {
                   mean = m
                 , variance = v
                 , cdfDenom = sqrt_2 * sv
                 , pdfDenom = sqrt_2_pi * sv
                 }
    where sv = sqrt v
                   
fromUArr :: UArr Double -> NormalDistribution
fromUArr a = fromParams (S.mean a) (S.var a)

probability d x = exp (-xm * xm / (2 * variance d)) / pdfDenom d
    where xm = x - mean d

cumulative :: NormalDistribution -> Double -> Double
cumulative d x = complementaryError (-(x-mean d) / cdfDenom d) / 2

inverse :: NormalDistribution -> Double -> Double
inverse d p
  | p == 0 = -D.huge
  | p == 1 = D.huge
  | p == 0.5 = mean d
  | otherwise = x * sqrt (variance d) + mean d
  where x = D.findRoot standard p 0 (-100) 100
