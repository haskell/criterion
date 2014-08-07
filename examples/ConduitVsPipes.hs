-- Contributed by Gabriel Gonzales as a test case for
-- https://github.com/bos/criterion/issues/35

import Criterion.Main
import Data.Conduit
import Data.Functor.Identity
import Pipes
import qualified Data.Conduit.List as C
import qualified Pipes.Prelude as P

criterion :: Int -> IO ()
criterion n = defaultMain
    [ bgroup "IO"
        [ bench "pipes"   $ nfIO (pipes   n)
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

main = criterion 10000
