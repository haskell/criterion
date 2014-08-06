{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
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

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Criterion.Types (Config)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random.MWC (GenIO, createSystemRandom)

data Crit = Crit {
    config :: {-# UNPACK #-} !Config
  , gen    :: {-# UNPACK #-} !(IORef (Maybe GenIO))
  }

-- | The monad in which most criterion code executes.
newtype Criterion a = Criterion {
      runCriterion :: ReaderT Crit IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader Config Criterion where
    ask     = config `fmap` Criterion ask
    local f = Criterion . local fconfig . runCriterion
      where fconfig c = c { config = f (config c) }

-- | Run a 'Criterion' action with the given 'Config'.
withConfig :: Config -> Criterion a -> IO a
withConfig cfg (Criterion act) = do
  g <- newIORef Nothing
  runReaderT act (Crit cfg g)

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
