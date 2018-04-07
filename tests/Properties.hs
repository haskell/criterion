{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties (tests) where

import Control.Applicative as A ((<$>))
import Criterion.Analysis
import Prelude ()
import Prelude.Compat
import Statistics.Types (Sample)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

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

tests :: TestTree
tests = testGroup "Properties" [
    testProperty "outlier_bucketing" outlier_bucketing
  , testProperty "outlier_bucketing_weighted" outlier_bucketing_weighted
  ]
