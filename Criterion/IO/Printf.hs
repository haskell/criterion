-- |
-- Module      : Criterion.IO.Printf
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
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
    , writeCsv
    ) where

import Control.Monad (when)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans (liftIO)
import Criterion.Monad (Criterion)
import Criterion.Types (Config(csvFile, verbosity), Verbosity(..))
import Data.Foldable (forM_)
import System.IO (Handle, hFlush, stderr, stdout)
import Text.Printf (PrintfArg)
import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as Csv
import qualified Text.Printf (HPrintfType, hPrintf)

-- First item is the action to print now, given all the arguments
-- gathered together so far.  The second item is the function that
-- will take a further argument and give back a new PrintfCont.
data PrintfCont = PrintfCont (IO ()) (forall a . PrintfArg a => a -> PrintfCont)

-- | An internal class that acts like Printf/HPrintf.
--
-- The implementation is visible to the rest of the program, but the
-- details of the class are not.
class CritHPrintfType a where
  chPrintfImpl :: (Config -> Bool) -> PrintfCont -> a


instance CritHPrintfType (Criterion a) where
  chPrintfImpl check (PrintfCont final _)
    = do x <- ask
         when (check x) (liftIO (final >> hFlush stderr >> hFlush stdout))
         return undefined

instance CritHPrintfType (IO a) where
  chPrintfImpl _ (PrintfCont final _)
    = final >> hFlush stderr >> hFlush stdout >> return undefined

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
note = chPrintf ((> Quiet) . verbosity) stdout

-- | Print verbose output.
prolix :: (CritHPrintfType r) => String -> r
prolix = chPrintf ((== Verbose) . verbosity) stdout

-- | Print an error message.
printError :: (CritHPrintfType r) => String -> r
printError = chPrintf (const True) stderr

-- | Write a record to a CSV file.
writeCsv :: Csv.ToRecord a => a -> Criterion ()
writeCsv val = do
  csv <- asks csvFile
  forM_ csv $ \fn ->
    liftIO . B.appendFile fn . Csv.encode $ [val]
