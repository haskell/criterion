{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main (bench, bgroup, env, whnf)
import System.Environment (getEnv, withArgs)
import System.Timeout (timeout)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)
import qualified Criterion.Main as C
import qualified Control.Exception as E
import qualified Data.ByteString as B

fib :: Int -> Int
fib = sum . go
  where go 0 = [0]
        go 1 = [1]
        go n = go (n-1) ++ go (n-2)

sanity :: Assertion
sanity = do
  args <- getArgEnv
  withArgs args $ do
    let tooLong = 30
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
             ]
    case wat of
      Just () -> return ()
      Nothing -> assertFailure $ "killed for running longer than " ++
                                 show tooLong ++ " seconds!"

main :: IO ()
main = defaultMain [testCase "sanity" sanity]

getArgEnv :: IO [String]
getArgEnv =
  fmap words (getEnv "ARGS") `E.catch`
  \(_ :: E.SomeException) -> return []
