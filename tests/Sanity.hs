{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if !MIN_VERSION_deepseq(1,4,2)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

import Criterion.Main (bench, bgroup, env, envWithCleanup, whnf, whnfIO)
import Control.Monad (unless)
import Data.IORef
import System.Environment (getEnv, withArgs)
import System.Timeout (timeout)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)
import qualified Criterion.Main as C
import qualified Control.Exception as E
import qualified Data.ByteString as B
#if !MIN_VERSION_deepseq(1,4,2)
import Control.DeepSeq
#endif

#if !MIN_VERSION_bytestring(0,10,0)
import Control.DeepSeq (NFData (..))
#endif

fib :: Int -> Int
fib = sum . go
  where go 0 = [0]
        go 1 = [1]
        go n = go (n-1) ++ go (n-2)

-- Additional arguments to include along with the ARGS environment variable.
extraArgs :: [String]
extraArgs = [ "--raw=sanity.dat", "--json=sanity.json", "--csv=sanity.csv"
            , "--output=sanity.html", "--junit=sanity.junit" ]

#if !MIN_VERSION_deepseq(1,4,2)
instance NFData (IORef a) where
  rnf x = x `seq` ()
#endif

sanity :: Assertion
sanity = do
  args <- getArgEnv
  withArgs (extraArgs ++ args) $ do
    let tooLong = 30
    stepVar <- newIORef (0::Int)
    wat <- timeout (tooLong * 1000000) $
           C.defaultMain [
               bgroup "fib" [
                 bench "fib 10" $ whnf fib 10
               , bench "fib 22" $ whnf fib 22
               ]
             , env (return (replicate 1024 0)) $ \xs ->
               bgroup "length . filter" [
                 bench "string" $ whnf (length . filter (==0)) xs
               , env (return (B.pack xs)) $ \bs ->
                 bench "bytestring" $ whnf (B.length . B.filter (==0)) bs
               ]
             , envWithCleanup
                 (modifyIORef' stepVar (+1) >> return stepVar)
                 (\var -> do
                     unless (var==stepVar) $
                       assertFailure $ "environment passed into cleanup " ++
                                       "is different from the one previously created"
                     modifyIORef' var (+1)
                 ) $ \var ->
                   bench "envWithCleanupCheck" $ whnfIO $ do
                     unless (var==stepVar) $
                       assertFailure $ "environment passed into benchmark " ++
                                       "is different from the one previously created"
                     step <- readIORef stepVar
                     unless (step==1) $
                       assertFailure $ "environment should be created exactly once"
             ]
    case wat of
      Just () -> do
        step <- readIORef stepVar
        unless (step==2) $
          assertFailure $ "environment was not properly cleaned up"
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
