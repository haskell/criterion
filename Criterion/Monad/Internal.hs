{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      : Criterion.Monad.Internal
-- Copyright   : (c) 2009 Neil Brown
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The environment in which most criterion code executes.
module Criterion.Monad.Internal
    (
      Criterion(..)
    , Crit(..)
    ) where

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import qualified Control.Monad.Fail as Fail (MonadFail(..))
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Trans (MonadIO, lift)
import Criterion.Types (Config)
import Data.IORef (IORef)
import Prelude ()
import Prelude.Compat
import System.Random.MWC (GenIO)

data Crit = Crit {
    config   :: !Config
  , gen      :: !(IORef (Maybe GenIO))
  }

-- | The monad in which most criterion code executes.
newtype Criterion a = Criterion {
      runCriterion :: ReaderT Crit IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadReader Config Criterion where
    ask     = config `fmap` Criterion ask
    local f = Criterion . local fconfig . runCriterion
      where fconfig c = c { config = f (config c) }

-- ReaderT has a MonadFail instance, but only on GHC 8.0+, so I inline the
-- fail implementation for ReaderT here to compensate. (Otherwise, I'd just
-- derive this instance.)
instance Fail.MonadFail Criterion where
  fail = Criterion . lift . Fail.fail
