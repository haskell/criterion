{-# LANGUAGE CPP #-}

-- |
-- Module      : Criterion.Main.Options.Internal
-- Copyright   : (c) 2022 Ryan Scott
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Provides a shim on top of @optparse-applicative@'s 'Options.tabulate'
-- function that is backwards-compatible with pre-@0.17.*@ versions of
-- @optparse-applicative@. This is deliberately kept separate from the rest of
-- "Criterion.Main.Options" because this function requires CPP to define, and
-- there is a Haddock comment in "Criterion.Main.Options" that will cause the
-- CPP preprocessor to trigger an \"unterminated comment\" error. Ugh.
--
-- TODO: When we support @optparse-applicative-0.17@ as the minimum, remove
-- this module and simply inline the definition of 'tabulate' in
-- "Criterion.Main.Options".
module Criterion.Main.Options.Internal (tabulate) where

import qualified Options.Applicative.Help as Options
import Options.Applicative.Help (Chunk, Doc)

#if MIN_VERSION_optparse_applicative(0,17,0)
import Options.Applicative (ParserPrefs(..), defaultPrefs)
#endif

-- | A shim on top of 'Options.tabulate' from @optparse-applicative@ that is
-- backwards-compatible with pre-@0.17.*@ versions of @optparse-applicative@.
tabulate :: [(Doc, Doc)] -> Chunk Doc
#if MIN_VERSION_optparse_applicative(0,17,0)
tabulate = Options.tabulate (prefTabulateFill defaultPrefs)
#else
tabulate = Options.tabulate
#endif
