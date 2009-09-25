module Criterion.IO
    (
      note
    , printError
    , prolix
    ) where

import Criterion.Config (Config, Verbosity(..), cfgVerbosity)
import System.IO (Handle, IOMode(..), openBinaryFile, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (HPrintfType, hPrintf)
import Prelude hiding (error)

nullDev :: Handle
nullDev = unsafePerformIO $ openBinaryFile "/dev/null" WriteMode
{-# NOINLINE nullDev #-}

note :: (HPrintfType r) => Config -> String -> r
note cfg msg = if cfgVerbosity cfg > Quiet
               then hPrintf stdout msg
               else hPrintf nullDev msg

prolix :: (HPrintfType r) => Config -> String -> r
prolix cfg msg = if cfgVerbosity cfg == Verbose
                 then hPrintf stdout msg
                 else hPrintf nullDev msg

printError :: (HPrintfType r) => String -> r
printError msg = hPrintf stderr msg
