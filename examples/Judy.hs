{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- cabal install judy.

import Criterion.Main
import Criterion.Config

import Control.Monad
import qualified Data.Judy as J
import qualified Data.IntMap as I
import Data.List

myConfig = defaultConfig { cfgPerformGC = ljust True }

main = defaultMainWith myConfig [
        bench "insert 1M"   (testit 1000000)
       ,bench "insert 10M"  (testit 10000000)
       ,bench "insert 100M" (testit 100000000)
    ]

testit n = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> J.insert n (fromIntegral n :: Int) j
   v <- J.lookup 100 j
   v `seq` return ()
