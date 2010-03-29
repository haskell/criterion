{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , getConfig
    , getConfigItem
    , withConfig
    ) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO)
import Criterion.Config (Config)

-- | The monad in which most criterion code executes.
newtype Criterion a = Criterion {
      runCriterion :: ReaderT Config IO a
    } deriving (Functor, Monad, MonadReader Config, MonadIO)

getConfig :: Criterion Config
getConfig = ask

getConfigItem :: (Config -> a) -> Criterion a
getConfigItem f = f `fmap` getConfig

withConfig :: Config -> Criterion a -> IO a
withConfig = flip (runReaderT . runCriterion)
