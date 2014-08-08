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
    , getOverhead
    ) where

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Criterion.Measurement (measure, runBenchmark, secs)
import Criterion.Monad.Internal (Criterion(..), Crit(..))
import Criterion.Types hiding (measure)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Statistics.Regression (olsRegress)
import System.Random.MWC (GenIO, createSystemRandom)
import qualified Data.Vector.Generic as G

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
getGen = memoise gen createSystemRandom

-- | Return an estimate of the measurement overhead.
getOverhead :: Criterion Double
getOverhead = do
  verbose <- asks ((> Quiet) . verbosity)
  memoise overhead $ do
    meas <- runBenchmark (whnfIO $ measure (whnfIO $ return ()) 1) 1
    let metric get = G.convert . G.map get $ meas
    let o = G.head . fst $
            olsRegress [metric (fromIntegral . measIters)] (metric measTime)
    when verbose . liftIO $
      putStrLn $ "measurement overhead " ++ secs o
    return o

-- | Memoise the result of an 'IO' action.
--
-- This is not currently thread-safe, but hopefully in a harmless way.
-- We might call the given action more than once if multiple threads
-- race, so our caller's job is to write actions that can be run
-- multiple times safely.
memoise :: (Crit -> IORef (Maybe a)) -> IO a -> Criterion a
memoise ref generate = do
  r <- Criterion $ asks ref
  liftIO $ do
    mv <- readIORef r
    case mv of
      Just rv -> return rv
      Nothing -> do
        rv <- generate
        writeIORef r (Just rv)
        return rv
