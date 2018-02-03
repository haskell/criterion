{-# OPTIONS_GHC -fno-full-laziness #-}
-- |
-- Module      : Criterion.Types.Internal
-- Copyright   : (c) 2017 Ryan Scott
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Exports 'fakeEnvironment'.
module Criterion.Types.Internal (fakeEnvironment, nf', whnf') where

import Data.Int (Int64)

-- | A dummy environment that is passed to functions that create benchmarks
-- from environments when no concrete environment is available.
fakeEnvironment :: env
fakeEnvironment = error $ unlines
  [ "Criterion atttempted to retrieve a non-existent environment!"
  , "\tPerhaps you forgot to use lazy pattern matching in a function which"
  , "\tconstructs benchmarks from an environment?"
  , "\t(see the documentation for `env` for details)"
  ]

-- | Return a function which applies an argument to a function a
-- given number of times, reducing the result to normal form.
nf' :: (b -> ()) -> (a -> b) -> a -> (Int64 -> IO ())
nf' reduce f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = let !y = f x
                       in reduce y `seq` go (n-1)
{-# NOINLINE nf' #-}

-- | Return a function which applies an argument to a function a
-- given number of times.
whnf' :: (a -> b) -> a -> (Int64 -> IO ())
whnf' f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf' #-}
