module VectorUtil where

import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Array.Vector
import System.Random.Mersenne

-- | Create an array, using the given action to populate each element.
createU :: (UA e) => Int -> (Int -> IO e) -> IO (UArr e)
createU size itemAt = assert (size >= 0) $
    unsafeSTToIO (newMU size) >>= loop 0
  where
    loop k arr | k >= size = unsafeSTToIO (unsafeFreezeAllMU arr)
               | otherwise = do
      r <- itemAt k
      unsafeSTToIO (writeMU arr k r)
      loop (k+1) arr

randomU :: (UA e, MTRandom e) => Int -> MTGen -> IO (UArr e)
randomU n gen = createU n . const $ random gen

