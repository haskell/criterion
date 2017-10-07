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
module Criterion.Types.Internal (fakeEnvironment) where

-- | A dummy environment that is passed to functions that create benchmarks
-- from environments when no concrete environment is available.
fakeEnvironment :: env
fakeEnvironment = error $ unlines
  [ "Criterion atttempted to retrieve a non-existent environment!"
  , "\tPerhaps you forgot to use lazy pattern matching in a function which"
  , "\tconstructs benchmarks from an environment?"
  , "\t(see the documentation for `env` for details)"
  ]
