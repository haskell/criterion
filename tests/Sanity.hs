{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE LinearTypes #-}
#endif
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if MIN_VERSION_base(4,16,0)
import Criterion.Main (bench, bgroup, env, whnf, lf)
import qualified Data.List.Linear as DLL
import qualified Prelude.Linear as PL
#else
import Criterion.Main (bench, bgroup, env, whnf)  
#endif
import System.Environment (getEnv, withArgs)
import System.Timeout (timeout)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)
import qualified Criterion.Main as C
import qualified Control.Exception as E
import qualified Data.ByteString as B

#if !MIN_VERSION_bytestring(0,10,0)
import Control.DeepSeq (NFData (..))
#endif

fib :: Int -> Int
fib = sum . go
  where go 0 = [0]
        go 1 = [1]
        go n = go (n-1) ++ go (n-2)
        
#if MIN_VERSION_base(4,16,0)
lfib :: PL.Int %1 -> PL.Int
lfib = DLL.sum PL.. go
  where go :: PL.Int %1 -> [PL.Int]
        -- go 0 = [0]
        -- go 1 = [1]
        go n = PL.dup n PL.& 
          \(np, nv) ->
            if np PL.< 2 
              then [nv]
              else PL.dup nv PL.& \(n', n'') -> go (n' PL.- 1) PL.++ go (n'' PL.- 2)
#endif

-- Additional arguments to include along with the ARGS environment variable.
extraArgs :: [String]
extraArgs = [ "--raw=sanity.dat", "--json=sanity.json", "--csv=sanity.csv"
            , "--output=sanity.html", "--junit=sanity.junit" ]

sanity :: Assertion
sanity = do
  args <- getArgEnv
  withArgs (extraArgs ++ args) $ do
    let tooLong = 30
    wat <- timeout (tooLong * 1000000) $
           C.defaultMain [
               bgroup "fib" [
                 bench "fib 10" $ whnf fib 10
               , bench "fib 22" $ whnf fib 22
#if MIN_VERSION_base(4,16,0)
               , bench "lfib 10" $ lf lfib 10  
#endif               
               ]
             , env (return (replicate 1024 0)) $ \xs ->
               bgroup "length . filter" [
                 bench "string" $ whnf (length . filter (==0)) xs
               , env (return (B.pack xs)) $ \bs ->
                 bench "bytestring" $ whnf (B.length . B.filter (==0)) bs
               ]
             ]
    case wat of
      Just () -> return ()
      Nothing -> assertFailure $ "killed for running longer than " ++
                                 show tooLong ++ " seconds!"

main :: IO ()
main = defaultMain $ testCase "sanity" sanity

-- This is a workaround to in pass arguments that sneak past
-- test-framework to get to criterion.
getArgEnv :: IO [String]
getArgEnv =
  fmap words (getEnv "ARGS") `E.catch`
  \(_ :: E.SomeException) -> return []

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString where
    rnf bs = bs `seq` ()
#endif
