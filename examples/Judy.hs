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
                     bench "insert 1M"   $ whnf testit 1000000
                   , bench "insert 10M"  $ whnf testit 10000000
                   , bench "insert 100M" $ whnf testit 100000000
                   ],
        bgroup "map" [
                     bench "insert 100k" $ whnf testmap 100000
                   , bench "insert 1M"   $ whnf testmap 1000000
                   ],
        bgroup "intmap" [
                     bench "insert 100k" $ whnf testintmap 100000
                   , bench "insert 1M"   $ whnf testintmap 1000000
                   ]
    ]

testit n = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> J.insert n (fromIntegral n :: Int) j
   v <- J.lookup 100 j
   v `seq` return ()

testmap :: Int -> M.Map Int Int
testmap n =
    foldl' (\m k -> M.insert k 1 m) M.empty [0..n]

testintmap :: Int -> I.IntMap Int
testintmap n =
    foldl' (\m k -> I.insert k 1 m) I.empty [0..n]
