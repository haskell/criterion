{-# LANGUAGE BangPatterns #-}

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
module Criterion.Measurement.Types.Internal
  ( fakeEnvironment
  , nf'
  , whnf'
  , SPEC(..)
  ) where

import Data.Int (Int64)
import Control.Exception
import GHC.Types (SPEC(..))

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
-- To ensure that the behavior of these functions remains independent of
-- -fspec-constr-count, we force SpecConst optimization by passing SPEC.
--
-- Finally, it's important that both branches of the loop depend on the state
-- token from the IO action. This is achieved by using `evaluate` rather than `let !y = f x`
-- in order to force the value to whnf. `evaluate` is in the IO monad and therefore the state
-- token needs to be passed through the loop.
--
-- See ghc#21948 where a change in eta-expansion behaviour
-- caused the work to be performed in the wrong place because the otherwise branch
-- did not depend on the state token at all, and the whole loop could be evaluated to
-- a single return function before being run in the IO monad.
--
-- See #183, #184 and #264 for discussion.

-- | Generate a function which applies an argument to a function a
-- given number of times, reducing the result to normal form.
nf' :: (b -> ()) -> (a -> b) -> a -> (Int64 -> IO ())
nf' reduce f x = go SPEC
  where
    go :: SPEC -> Int64 -> IO ()
    go !_ n
      | n <= 0    = return ()
      | otherwise = do
         y <- evaluate (f x)
         reduce y `seq` go SPEC (n-1)
{-# NOINLINE nf' #-}

-- | Generate a function which applies an argument to a function a
-- given number of times.
whnf' :: (a -> b) -> a -> (Int64 -> IO ())
whnf' f x = go SPEC
  where
    go :: SPEC -> Int64 -> IO ()
    go !_ n
      | n <= 0    = return ()
      | otherwise = do
         _ <- evaluate (f x)
         go SPEC (n-1)
{-# NOINLINE whnf' #-}
