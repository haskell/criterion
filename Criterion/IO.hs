-- |
-- Module      : Criterion.IO
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Input and output actions.

module Criterion.IO
    (
      NoOp
    , note
    , printError
    , prolix
    , summary
    ) where

import Criterion.Config (Config, Verbosity(..), cfgSummaryFile, cfgVerbosity, fromLJ)
import Data.Monoid (getLast)
import System.IO (stderr, stdout)
import Text.Printf (HPrintfType, hPrintf)

-- | A typeclass hack to match that of the 'HPrintfType' class.
class NoOp a where
    noop :: a

instance NoOp (IO a) where
    noop = return undefined

instance (NoOp r) => NoOp (a -> r) where
    noop _ = noop

-- | Print a \"normal\" note.
note :: (HPrintfType r, NoOp r) => Config -> String -> r
note cfg msg = if fromLJ cfgVerbosity cfg > Quiet
               then hPrintf stdout msg
               else noop

-- | Print verbose output.
prolix :: (HPrintfType r, NoOp r) => Config -> String -> r
prolix cfg msg = if fromLJ cfgVerbosity cfg == Verbose
                 then hPrintf stdout msg
                 else noop

-- | Print an error message.
printError :: (HPrintfType r) => String -> r
printError msg = hPrintf stderr msg

-- | Add to summary CSV (if applicable)
summary :: Config -> String -> IO ()
summary cfg msg = case getLast $ cfgSummaryFile cfg of
  Just fn -> appendFile fn msg
  Nothing -> return ()
