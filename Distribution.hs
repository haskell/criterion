{-# LANGUAGE BangPatterns #-}
module Distribution where

class Distribution d where
    probability :: d -> Double -> Double
    cumulative  :: d -> Double -> Double
    inverse     :: d -> Double -> Double

huge :: Double
huge = 1e308

-- | Approximate the value of @X@ for which @P(x>X) == p@.
--
-- This method uses a combination of Newton-Raphson iteration and
-- bisection with the given guess as a starting point.  The upper and
-- lower bounds specify the interval in which the probability
-- distribution reaches the value @p@.
findRoot :: Distribution d => d
         -> Double              -- ^ Probability @p@
         -> Double              -- ^ Initial guess
         -> Double              -- ^ Lower bound on interval
         -> Double              -- ^ Upper bound on interval
         -> Double
findRoot d prob = loop 0 1
  where
    loop !i !dx !x !lo !hi
      | abs dx <= accuracy || i >= maxIters = x
      | otherwise                           = loop (i+1) dx'' x'' lo' hi'
      where
        err                   = cumulative d x - prob
        (lo',hi') | err < 0   = (x, hi)
                  | otherwise = (lo, x)
        pdf                   = probability d x
        (dx',x') | pdf /= 0   = (err / pdf, x - dx)
                 | otherwise  = (dx, x)
        (dx'',x'')
            | x' < lo' || x' > hi' || pdf == 0 = (x'-x, (lo + hi) / 2)
            | otherwise                        = (dx',  x')
    accuracy = 1e-15
    maxIters = 150
