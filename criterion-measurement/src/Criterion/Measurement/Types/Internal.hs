{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE LinearTypes #-}
#endif

-- Ensure that nf' and whnf' are always optimized, even if
-- criterion-measurement is compiled with -O0 or -fprof-auto (see #184).
{-# OPTIONS_GHC -O2 -fno-prof-auto #-}
-- Make the function applications in nf' and whnf' strict (avoiding allocation)
-- and avoid floating out the computations.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- |
-- Module      : Criterion.Measurement.Types.Internal
-- Copyright   : (c) 2017 Ryan Scott
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Exports 'fakeEnvironment'.

#if MIN_VERSION_base(4,16,0)
module Criterion.Measurement.Types.Internal (fakeEnvironment, nf', lf', whnf') where
#else
module Criterion.Measurement.Types.Internal (fakeEnvironment, nf', whnf') where
#endif    

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

-- Along with Criterion.Types.nfIO' and Criterion.Types.whnfIO', the following
-- two functions are the core benchmarking loops. They have been carefully
-- constructed to avoid allocation while also evaluating @f x@.
--
-- Because these functions are pure, GHC is particularly smart about optimizing
-- them. We must turn off @-ffull-laziness@ to prevent the computation from
-- being floated out of the loop.
--
-- For a similar reason, these functions must not be inlined. There are two
-- possible issues that can arise if they are inlined. First, the work is often
-- floated out of the loop, which creates a nonsense benchmark. Second, the
-- benchmark code itself could be changed by the user's optimization level. By
-- marking them @NOINLINE@, the core benchmark code is always the same.
--
-- See #183 and #184 for discussion.

-- | Generate a function which applies an argument to a function a
-- given number of times, reducing the result to normal form.
nf' :: (b -> ()) -> (a -> b) -> a -> (Int64 -> IO ())
nf' reduce f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = let !y = f x
                       in reduce y `seq` go (n-1)
{-# NOINLINE nf' #-}

#if MIN_VERSION_base(4,16,0)
lf' :: (b -> ()) -> (a %1 -> b) -> a -> (Int64 -> IO ())
lf' reduce f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = let !y = f x
                       in reduce y `seq` go (n-1)
{-# NOINLINE lf' #-}
#endif

-- | Generate a function which applies an argument to a function a
-- given number of times.
whnf' :: (a -> b) -> a -> (Int64 -> IO ())
whnf' f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = f x `seq` go (n-1)
{-# NOINLINE whnf' #-}
