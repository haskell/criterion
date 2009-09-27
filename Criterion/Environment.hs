{-# LANGUAGE DeriveDataTypeable, TypeOperators #-}

-- |
-- Module      : Criterion.Environment
-- Copyright   : (c) Bryan O'Sullivan 2009
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
import Criterion.Analysis (analyseMean)
import Criterion.Config (Config)
import Criterion.IO (note)
import Criterion.Measurement (getTime, runForAtLeast, time_)
import Data.Array.Vector
import Data.Typeable (Typeable)
import Statistics.Function (createIO)

-- | Measured aspects of the execution environment.
data Environment = Environment {
      envClockResolution :: {-# UNPACK #-} !Double
    -- ^ Clock resolution (in seconds).
    , envClockCost       :: {-# UNPACK #-} !Double
    -- ^ The cost of a single clock call (in seconds).
    } deriving (Eq, Read, Show, Typeable)

-- | Measure the execution environment.
measureEnvironment :: Config -> IO Environment
measureEnvironment cfg = do
  note cfg "warming up\n"
  (_ :*: seed :*: _) <- runForAtLeast 0.1 10000 resolution
  note cfg "estimating clock resolution...\n"
  clockRes <- thd3 `fmap` runForAtLeast 0.5 seed resolution >>=
              uncurry (analyseMean cfg)
  note cfg "estimating cost of a clock call...\n"
  clockCost <- cost (min (100000 * clockRes) 1) >>= uncurry (analyseMean cfg)
  return $ Environment {
               envClockResolution = clockRes
             , envClockCost = clockCost
             }
  where
    resolution k = do
      times <- createIO (k+1) (const getTime)
      return (tailU . filterU (>=0) . zipWithU (-) (tailU times) $ times,
              lengthU times)
    cost timeLimit = do
      let timeClock k = time_ (replicateM_ k getTime)
      timeClock 1
      (_ :*: iters :*: elapsed) <- runForAtLeast 0.01 10000 timeClock
      times <- createIO (ceiling (timeLimit / elapsed)) $ \_ -> timeClock iters
      return (mapU (/ fromIntegral iters) times, lengthU times)
    thd3 (_ :*: _:*: c) = c
