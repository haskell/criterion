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
-- Provides a shim on top of @optparse-applicative@ to define two functions:
--
-- * Define a 'tabulate' function that is backwards-compatible with
--   pre-@0.17.*@ versions of @optparse-applicative@.
-- * Define a 'text' function that is forward-compatible with
--   @optparse-applicative-0.18.*@ or later.
--
-- These are deliberately kept separate from the rest of
-- "Criterion.Main.Options" because these functions require CPP to define, and
-- there is a Haddock comment in "Criterion.Main.Options" that will cause the
-- CPP preprocessor to trigger an \"unterminated comment\" error. Ugh.
--
-- TODO: When we support @optparse-applicative-0.18@ as the minimum, remove
-- this module, and simply inline the definitions of 'tabulate' and 'text' in
-- "Criterion.Main.Options".
module Criterion.Main.Options.Internal (tabulate, text) where

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

-- | A shim on top of 'Options.pretty' from @optparse-applicative@ that is
-- forward-compatible with @optparse-applicative-0.18.*@ or later.
text :: String -> Doc
#if MIN_VERSION_optparse_applicative(0,18,0)
text = Options.pretty
#else
text = Options.text
#endif
