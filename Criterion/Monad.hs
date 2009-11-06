{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Criterion.Monad
-- Copyright   : (c) Neil Brown 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The environment in which most criterion code executes.
module Criterion.Monad
    (
      ConfigM
    , getConfig
    , getConfigItem
    , withConfig
    ) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO)
import Criterion.Config (Config)

-- | The monad in which most criterion code executes.
newtype ConfigM a = ConfigM {
      runConfigM :: ReaderT Config IO a
    } deriving (Functor, Monad, MonadReader Config, MonadIO)

getConfig :: ConfigM Config
getConfig = ask

getConfigItem :: (Config -> a) -> ConfigM a
getConfigItem f = f `fmap` getConfig

withConfig :: Config -> ConfigM a -> IO a
withConfig = flip (runReaderT . runConfigM)
