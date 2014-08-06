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
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO)
import Criterion.Types (Config)

data Crit = Crit {
    config :: {-# UNPACK #-} !Config
  }

-- | The monad in which most criterion code executes.
newtype Criterion a = Criterion {
      runCriterion :: ReaderT Crit IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader Config Criterion where
    ask     = config `fmap` Criterion ask
    local f = Criterion . local (Crit . f . config) . runCriterion

-- | Run a 'Criterion' action with the given 'Config'.
withConfig :: Config -> Criterion a -> IO a
withConfig cfg (Criterion act) = runReaderT act (Crit cfg)
