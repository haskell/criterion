module SpecialMath where

import Data.Array.Vector
import Prelude hiding (error)

-- FIXME: The definitions of error and complementaryError are only
-- accurate to a few decimal places due to a poor choice of
-- approximation.

-- Compute the complementary error function of @x@, defined as
-- follows:
--
-- > 1 - error x
--
-- This avoids the loss in accuracy that would occur for the
-- calculation @1 - error x@ for large values of @x@, when the value
-- of @error x@ approaches 1.
complementaryError :: Double -> Double
complementaryError x
    | x >= 0    = r
    | otherwise = 2 - r
  where
    r | ax < 1.25 = 1 - error ax
      | ax > 28   = 0
      | otherwise = exp (-xx - 0.5625 + rr / ss) / ax
    ax = abs x
    xx = x * x
    s  = 1 / xx
    (rr,ss) | ax < 1 / 0.35 = (reduce s eRa, 1 + reduce s eSa)
            | otherwise     = (reduce s eRb, 1 + reduce s eSb)

-- | Compute the error function of @x@, defined as follows
--
-- > 2/sqrt(pi)* integral from 0 to x of exp(-t*t) dt
error :: Double -> Double
error x = r * signum x
  where
    r | ax < 3.7252902984619141e-9 = ax + ax * e_efx
      | ax < 0.84375 = let p = reduce s ePp
                           q = 1 + reduce s eQq
                           s = x*x
                       in ax + ax * p/q
      | ax < 1.25 = let p = reduce s ePa
                        q = 1 + reduce s eQa
                        s = ax - 1
                    in e_erx + p/q
      | ax >= 6   = 1
      | otherwise = 1 - complementaryError ax
    ax    = abs x
    e_efx = 1.28379167095512586316e-01
    e_erx = 8.45062911510467529297e-01

reduce :: Double -> UArr Double -> Double
reduce s = foldr1U (\a b -> a + s * b)

-- Coefficients for approximation to erfc in [1.25,1/.35].

eRa :: UArr Double
eRa = toU [ -9.86494403484714822705e-03, -6.93858572707181764372e-01
          , -1.05586262253232909814e01, -6.23753324503260060396e01
          , -1.62396669462573470355e02, -1.84605092906711035994e02
          , -8.12874355063065934246e01, -9.81432934416914548592e00 ]

eSa :: UArr Double
eSa = toU [ 1.96512716674392571292e01, 1.37657754143519042600e02
          , 4.34565877475229228821e02, 6.45387271733267880336e02
          , 4.29008140027567833386e02, 1.08635005541779435134e02
          , 6.57024977031928170135e00, -6.04244152148580987438e-02 ]

-- Coefficients for approximation to erfc in [1/.35,28].

eRb :: UArr Double
eRb = toU [ -9.86494292470009928597e-03, -7.99283237680523006574e-01
          , -1.77579549177547519889e01, -1.60636384855821916062e02
          , -6.37566443368389627722e02, -1.02509513161107724954e03
          , -4.83519191608651397019e02 ]

eSb :: UArr Double
eSb = toU [ 3.03380607434824582924e01, 3.25792512996573918826e02
          , 1.53672958608443695994e03, 3.19985821950859553908e03
          , 2.55305040643316442583e03, 4.74528541206955367215e02
          , -2.24409524465858183362e01 ]

-- Coefficients for approximation to erf in [0,0.84375].

ePp :: UArr Double
ePp = toU [ 1.28379167095512558561e-01, -3.25042107247001499370e-01
          , -2.84817495755985104766e-02, -5.77027029648944159157e-03
          , -2.37630166566501626084e-05 ]

eQq :: UArr Double
eQq = toU [ 3.97917223959155352819e-01, 6.50222499887672944485e-02
          , 5.08130628187576562776e-03, 1.32494738004321644526e-04
          , -3.96022827877536812320e-06 ]

-- Coefficients for approximation to erf in [0.84375,1.25].

ePa :: UArr Double
ePa = toU [ -2.36211856075265944077e-03, 4.14856118683748331666e-01
          , -3.72207876035701323847e-01, 3.18346619901161753674e-01
          , -1.10894694282396677476e-01, 3.54783043256182359371e-02
          , -2.16637559486879084300e-03 ]

eQa :: UArr Double
eQa = toU [ 1.06420880400844228286e-01, 5.40397917702171048937e-01
          , 7.18286544141962662868e-02, 1.26171219808761642112e-01
          , 1.36370839120290507362e-02, 1.19844998467991074170e-02 ]
