-- Benchmark the cost of creating various types of map.

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

import Criterion.Main
import Data.ByteString (ByteString, pack)
import Data.Hashable (Hashable)
import System.Random.MWC
import qualified Data.HashMap.Lazy as H
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

type V = U.Vector Int
type B = V.Vector ByteString

numbers :: IO (V, V, V)
numbers = do
  gen <- createSystemRandom
  random <- uniformVector gen 40000
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

strings :: IO (B, B, B)
strings = do
  gen <- createSystemRandom
  random <- V.replicateM 10000 $
      (pack . U.toList) `fmap` (uniformVector gen =<< uniformR (1,16) gen)
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

main :: IO ()
main = defaultMain [
         env numbers $ \ ~(random,sorted,revsorted) ->
         bgroup "Int" [
           bgroup "IntMap" [
             bench "sorted"    $ whnf intmap sorted
           , bench "random"    $ whnf intmap random
           , bench "revsorted" $ whnf intmap revsorted
           ]
         , bgroup "Map" [
             bench "sorted"    $ whnf mmap sorted
           , bench "random"    $ whnf mmap random
           , bench "revsorted" $ whnf mmap revsorted
           ]
         , bgroup "HashMap" [
             bench "sorted"    $ whnf hashmap sorted
           , bench "random"    $ whnf hashmap random
           , bench "revsorted" $ whnf hashmap revsorted
           ]
         ]
       , env strings $ \ ~(random,sorted,revsorted) ->
         bgroup "ByteString" [
           bgroup "Map" [
             bench "sorted"    $ whnf mmap sorted
           , bench "random"    $ whnf mmap random
           , bench "revsorted" $ whnf mmap revsorted
           ]
         , bgroup "HashMap" [
             bench "sorted"    $ whnf hashmap sorted
           , bench "random"    $ whnf hashmap random
           , bench "revsorted" $ whnf hashmap revsorted
           ]
         ]
       ]

hashmap :: (G.Vector v k, Hashable k, Eq k) => v k -> H.HashMap k Int
hashmap xs = G.foldl' (\m k -> H.insert k value m) H.empty xs

intmap :: G.Vector v Int => v Int -> I.IntMap Int
intmap xs = G.foldl' (\m k -> I.insert k value m) I.empty xs

mmap :: (G.Vector v k, Ord k) => v k -> M.Map k Int
mmap xs = G.foldl' (\m k -> M.insert k value m) M.empty xs

value :: Int
value = 31337
