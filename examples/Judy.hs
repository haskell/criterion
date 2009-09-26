{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

-- cabal install judy

import Control.Monad (forM_)
import Criterion.Config
import Criterion.Main
import qualified Data.IntMap as I
import qualified Data.Judy as J

-- Work around the fact that the GC won't run finalizers aggressively
-- enough for us.
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
