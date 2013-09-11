{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties (tests) where

import Control.Applicative ((<$>))
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
  arbitrary = U.fromList <$> arbitrary
  shrink    = map U.fromList . shrink . U.toList

outlier_bucketing :: Sample -> Bool
outlier_bucketing xs =
  countOutliers (classifyOutliers xs) <= fromIntegral (G.length xs)

outlier_bucketing_weighted :: Sample -> Bool
outlier_bucketing_weighted xs =
  outlier_bucketing (xs <> G.replicate (G.length xs * 10) 0)

tests :: Test
tests = testGroup "Properties" [
    testProperty "outlier_bucketing" outlier_bucketing
  , testProperty "outlier_bucketing_weighted" outlier_bucketing_weighted
  ]
