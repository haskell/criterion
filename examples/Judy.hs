{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

-- cabal install judy

import Control.Monad (forM_)
import Criterion.Config
import Criterion.Main
import Criterion.Types
import qualified Data.IntMap as I
import qualified Data.Judy as J
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.List (foldl')

-- Work around the fact that the GC won't run finalizers aggressively
-- enough for us.
myConfig = defaultConfig { cfgPerformGC = ljust True }

main = defaultMainWith myConfig [
        bgroup "judy" [
                     bench "insert 1M"   (testit 1000000)
                   , bench "insert 10M"  (testit 10000000)
                   , bench "insert 100M" (testit 100000000)
                   ],
        bgroup "map" [
                      bench "insert 100k" (testmap 100000)
                   , bench "insert 1M"    (testmap 1000000)
                   ],
        bgroup "intmap" [
                     bench "insert 100k" (testintmap 100000)
                   , bench "insert 1M"   (testintmap 1000000)
                   ]
    ]

testit n = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> J.insert n (fromIntegral n :: Int) j
   v <- J.lookup 100 j
   v `seq` return ()

testmap :: Int -> Int -> M.Map Int Int
testmap n i =
    foldl' (\m k -> M.insert k 1 m) M.empty [0..(n+i-i)]

testintmap :: Int -> Int -> I.IntMap Int
testintmap n i =
    foldl' (\m k -> I.insert k 1 m) I.empty [0..(n+i-i)]
