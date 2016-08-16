{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties (tests) where

import Control.Applicative as A ((<$>))
import Criterion.Analysis
import Statistics.Types (Sample)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
    
#if __GLASGOW_HASKELL__ >= 704
import Data.Monoid ((<>))
#else
import Data.Monoid

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
infixr 6 <>
#endif

instance (Arbitrary a, U.Unbox a) => Arbitrary (U.Vector a) where
  arbitrary = U.fromList A.<$> arbitrary
  shrink    = map U.fromList . shrink . U.toList

outlier_bucketing :: Double -> Sample -> Bool
outlier_bucketing y ys =
  countOutliers (classifyOutliers xs) <= fromIntegral (G.length xs)
  where xs = U.cons y ys

outlier_bucketing_weighted :: Double -> Sample -> Bool
outlier_bucketing_weighted x xs =
  outlier_bucketing x (xs <> G.replicate (G.length xs * 10) 0)

tests :: Test
tests = testGroup "Properties" [
    testProperty "outlier_bucketing" outlier_bucketing
  , testProperty "outlier_bucketing_weighted" outlier_bucketing_weighted
  ]
