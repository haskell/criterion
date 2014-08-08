-- Contributed by Gabriel Gonzales as a test case for
-- https://github.com/bos/criterion/issues/35
--
-- The numbers reported by this benchmark can be made "more correct"
-- by compiling with the -fno-full-laziness option.

import Criterion.Main (bench, bgroup, defaultMain, nfIO, whnf)
import Data.Conduit (($=), ($$))
import Data.Functor.Identity (Identity(..))
import Pipes ((>->), discard, each, for, runEffect)
import qualified Data.Conduit.List as C
import qualified Pipes.Prelude as P

criterion :: Int -> IO ()
criterion n = defaultMain
    [ bgroup "IO"
        [ -- This will appear to run in just a few nanoseconds.
          bench "pipes"   $ nfIO (pipes   n)
          -- In contrast, this should take ~10 microseconds.  Which is
          -- also wrong, as it happens.
        , bench "conduit" $ nfIO (conduit n)
        ]
    , bgroup "Identity"
        [ bench "pipes"   $ whnf (runIdentity . pipes  ) n
        , bench "conduit" $ whnf (runIdentity . conduit) n
        ]
    ]

pipes, conduit :: (Monad m) => Int -> m ()
pipes n = runEffect $ for (each [1..n] >-> P.map (+1) >-> P.filter even) discard
conduit n = C.sourceList [1..n] $= C.map (+1) $= C.filter even $$ C.sinkNull

main :: IO ()
main = criterion 10000
