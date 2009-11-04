-- |
-- Module      : Criterion.Monad
-- Copyright   : (c) Neil Brown 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
module Criterion.Monad (ConfigM, getConfig, getConfigItem, doIO, withConfig) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Criterion.Config (Config)

type ConfigM = ReaderT Config IO

getConfig :: ConfigM Config
getConfig = ask

getConfigItem :: (Config -> a) -> ConfigM a
getConfigItem f = f `fmap` getConfig

doIO :: IO a -> ConfigM a
doIO = lift

withConfig :: Config -> ConfigM a -> IO a
withConfig = flip runReaderT
