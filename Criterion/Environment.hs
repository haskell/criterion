{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TypeOperators #-}

-- |
-- Module      : Criterion.Environment
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Code for measuring and characterising the execution environment.

module Criterion.Environment
    (
      Environment(..)
    , measureEnvironment
    ) where

import Control.Monad (replicateM_)
import Control.Monad.Trans (liftIO)
import Criterion.Analysis (analyseMean)
import Criterion.IO.Printf (note)
import Criterion.Measurement (getTime, runForAtLeast, time_)
import Criterion.Monad (Criterion)
import qualified Data.Vector.Unboxed as U
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

-- | Measured aspects of the execution environment.
data Environment = Environment {
      envClockResolution :: {-# UNPACK #-} !Double
    -- ^ Clock resolution (in seconds).
    , envClockCost       :: {-# UNPACK #-} !Double
    -- ^ The cost of a single clock call (in seconds).
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Measure the execution environment.
measureEnvironment :: Criterion Environment
measureEnvironment = do
  _ <- note "warming up\n"
  (_, seed, _) <- liftIO $ runForAtLeast 0.1 10000 resolution
  _ <- note "estimating clock resolution...\n"
  clockRes <- thd3 `fmap` liftIO (runForAtLeast 0.5 seed resolution) >>=
              uncurry analyseMean
  _ <- note "estimating cost of a clock call...\n"
  clockCost <- cost (min (100000 * clockRes) 1) >>= uncurry analyseMean
  return $ Environment {
               envClockResolution = clockRes
             , envClockCost = clockCost
             }
  where
    resolution k = do
      times <- U.replicateM (k+1) getTime
      return (U.tail . U.filter (>=0) . U.zipWith (-) (U.tail times) $ times,
              U.length times)
    cost timeLimit = liftIO $ do
      let timeClock k = time_ (replicateM_ k getTime)
      _ <- timeClock 1
      (_, iters, elapsed) <- runForAtLeast 0.01 10000 timeClock
      times <- U.replicateM (ceiling (timeLimit / elapsed)) $ timeClock iters
      return (U.map (/ fromIntegral iters) times, U.length times)
    thd3 (_, _, c) = c
