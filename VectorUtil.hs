module VectorUtil where

import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Array.Vector
import System.Random.Mersenne

randomU :: (UA e, MTRandom e) => Int -> MTGen -> IO (UArr e)
randomU n gen = createU n . const $ random gen

