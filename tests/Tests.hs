module Main (main) where



import Criterion.Types
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import Properties
import Statistics.Resampling.Bootstrap (Estimate(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit

r1 :: Report
r1 = Report 0 "" [] v1 s1 (Outliers 0 0 0 0 0) []
 where 
  m1 = Measured 4.613000783137977e-05 3.500000000000378e-05 31432 1 0 0 0 0.0 0.0 0.0 0.0
  v1 = V.fromList [m1]
  est1 = Estimate 0.0 0.0 0.0 0.0
  s1 = SampleAnalysis [] 0.0 est1 est1 (OutlierVariance Unaffected "" 0.0)

m2 :: Measured
m2 = Measured {measTime = 1.1438998626545072e-5
              , measCpuTime = 1.2000000001677336e-5
              , measCycles = 6208
              , measIters = 1

              , measAllocated = -9223372036854775808
              , measNumGcs = -9223372036854775808
              , measBytesCopied = -9223372036854775808

              , measMutatorWallSeconds = -1/0
              , measMutatorCpuSeconds = -1/0
              , measGcWallSeconds = -1/0
              , measGcCpuSeconds = -1/0}

main :: IO ()
main = defaultMain
       [ Properties.tests
       , testCase "json-roundtrip1"
           (assertEqual "round trip simple Measured"
              (Right m2) (Aeson.eitherDecode (Aeson.encode m2)))
       , testCase "json-roundtrip2"
           (assertEqual "round trip simple Report"
              (Right r1) (Aeson.eitherDecode (Aeson.encode r1)))
       ]
