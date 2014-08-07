-- |
-- Module      : Criterion.Monad
-- Copyright   : (c) 2009 Neil Brown
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The environment in which most criterion code executes.
module Criterion.Monad
    (
      Criterion
    , withConfig
    , getGen
    ) where

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Trans (liftIO)
import Criterion.Monad.Internal (Criterion(..), Crit(..))
import Criterion.Types (Config)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Random.MWC (GenIO, createSystemRandom)

-- | Run a 'Criterion' action with the given 'Config'.
withConfig :: Config -> Criterion a -> IO a
withConfig cfg (Criterion act) = do
  g <- newIORef Nothing
  o <- newIORef Nothing
  runReaderT act (Crit cfg g o)

-- | Return a random number generator, creating one if necessary.
--
-- This is not currently thread-safe, but in a harmless way (we might
-- call 'createSystemRandom' more than once if multiple threads race).
getGen :: Criterion GenIO
getGen = do
  g <- Criterion $ asks gen
  liftIO $ do
    mg <- readIORef g
    case mg of
      Just rg -> return rg
      Nothing -> do
        rg <- createSystemRandom
        writeIORef g (Just rg)
        return rg
