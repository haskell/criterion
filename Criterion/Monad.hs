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
    , withConfig
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO)
import Criterion.Types (Config)

-- | The monad in which most criterion code executes.
newtype Criterion a = Criterion {
      runCriterion :: ReaderT Config IO a
    } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

withConfig :: Config -> Criterion a -> IO a
withConfig = flip (runReaderT . runCriterion)
