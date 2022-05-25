{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE LinearTypes #-}

module  Criterion.Measurement.Types.Linear (nf, whnf, nfAppIO, whnfAppIO) where

import Control.DeepSeq (NFData)
import Criterion.Measurement.Types (Benchmarkable, nfLinear, whnfLinear, nfAppIoLinear, whnfAppIoLinear)

-- | linear variant of 'nf'
nf :: NFData b => (a %1 -> b) -> a -> Benchmarkable
nf = nfLinear

-- | linear variant of 'whnf'
whnf :: (a %1 -> b) -> a -> Benchmarkable
whnf = whnfLinear

nfAppIO :: NFData b => (a %1 -> IO b) -> a -> Benchmarkable
nfAppIO = nfAppIoLinear

whnfAppIO :: (a %1 -> IO b) -> a -> Benchmarkable
whnfAppIO = whnfAppIoLinear

#else
  
module Criterion.Measurement.Types.Linear () where

nop :: ()
nop = ()

#endif
