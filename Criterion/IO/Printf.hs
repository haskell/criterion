-- |
-- Module      : Criterion.IO.Printf
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Input and output actions.

{-# LANGUAGE FlexibleInstances, Rank2Types, TypeSynonymInstances #-}
module Criterion.IO.Printf
    (
      CritHPrintfType
    , note
    , printError
    , prolix
    , summary
    ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Criterion.Config (Config, Verbosity(..), cfgSummaryFile, cfgVerbosity, fromLJ)
import Criterion.Monad (Criterion, getConfig, getConfigItem)
import Data.Monoid (getLast)
import System.IO (Handle, stderr, stdout)
import qualified Text.Printf (HPrintfType, hPrintf)
import Text.Printf (PrintfArg)

-- First item is the action to print now, given all the arguments
-- gathered together so far.  The second item is the function that
-- will take a further argument and give back a new PrintfCont.
data PrintfCont = PrintfCont (IO ()) (PrintfArg a => a -> PrintfCont)

-- | An internal class that acts like Printf/HPrintf.
--
-- The implementation is visible to the rest of the program, but the
-- details of the class are not.
class CritHPrintfType a where
  chPrintfImpl :: (Config -> Bool) -> PrintfCont -> a


instance CritHPrintfType (Criterion a) where
  chPrintfImpl check (PrintfCont final _)
    = do x <- getConfig
         when (check x) (liftIO final)
         return undefined

instance CritHPrintfType (IO a) where
  chPrintfImpl _ (PrintfCont final _)
    = final >> return undefined

instance (CritHPrintfType r, PrintfArg a) => CritHPrintfType (a -> r) where
  chPrintfImpl check (PrintfCont _ anotherArg) x
    = chPrintfImpl check (anotherArg x)

chPrintf :: CritHPrintfType r => (Config -> Bool) -> Handle -> String -> r
chPrintf shouldPrint h s
  = chPrintfImpl shouldPrint (make (Text.Printf.hPrintf h s)
                                   (Text.Printf.hPrintf h s))
  where
    make :: IO () -> (forall a r. (PrintfArg a, Text.Printf.HPrintfType r) =>
                      a -> r) -> PrintfCont
    make curCall curCall' = PrintfCont curCall (\x -> make (curCall' x)
                                                      (curCall' x))

{- A demonstration of how to write printf in this style, in case it is
ever needed
  in fututre:

cPrintf :: CritHPrintfType r => (Config -> Bool) -> String -> r
cPrintf shouldPrint s
  = chPrintfImpl shouldPrint (make (Text.Printf.printf s)
  (Text.Printf.printf s))
  where
    make :: IO () -> (forall a r. (PrintfArg a, Text.Printf.PrintfType r) => a -> r) -> PrintfCont
    make curCall curCall' = PrintfCont curCall (\x -> make (curCall' x) (curCall' x))
-}

-- | Print a \"normal\" note.
note :: (CritHPrintfType r) => String -> r
note = chPrintf ((> Quiet) . fromLJ cfgVerbosity) stdout

-- | Print verbose output.
prolix :: (CritHPrintfType r) => String -> r
prolix = chPrintf ((== Verbose) . fromLJ cfgVerbosity) stdout

-- | Print an error message.
printError :: (CritHPrintfType r) => String -> r
printError = chPrintf (const True) stderr

-- | Add to summary CSV (if applicable)
summary :: String -> Criterion ()
summary msg
  = do sumOpt <- getConfigItem (getLast . cfgSummaryFile)
       case sumOpt of
         Just fn -> liftIO $ appendFile fn msg
         Nothing -> return ()
