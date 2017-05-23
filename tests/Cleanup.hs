{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main (Benchmark, bench, nfIO)
import Criterion.Types (Config(..), Verbosity(Quiet))
import Control.Applicative (pure)
import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, try, throwIO)
import Control.Monad (when)
import Data.Typeable (Typeable)
import System.Directory (doesFileExist, removeFile)
import System.Environment (withArgs)
import System.IO ( Handle, IOMode(ReadWriteMode), SeekMode(AbsoluteSeek)
                 , hClose, hFileSize, hSeek, openFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (assertFailure)
import qualified Criterion.Main as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

instance NFData Handle where
    rnf !_ = ()

data CheckResult = ShouldThrow | WrongData deriving (Show, Typeable)

instance Exception CheckResult

type BenchmarkWithFile =
  String -> IO Handle -> (Handle -> IO ()) -> (Handle -> IO ()) -> Benchmark

perRun :: BenchmarkWithFile
perRun name alloc clean work =
  bench name $ C.perRunEnvWithCleanup alloc clean work

perBatch :: BenchmarkWithFile
perBatch name alloc clean work =
  bench name $ C.perBatchEnvWithCleanup (const alloc) (const clean) work

envWithCleanup :: BenchmarkWithFile
envWithCleanup name alloc clean work =
  C.envWithCleanup alloc clean $ bench name . nfIO . work

testCleanup :: Bool -> String -> BenchmarkWithFile -> TestTree
testCleanup shouldFail name withEnvClean = testCase name $ do
    existsBefore <- doesFileExist testFile
    when existsBefore $ failTest "Input file already exists"

    result <- runTest . withEnvClean name alloc clean $ \hnd -> do
        result <- hFileSize hnd >>= BS.hGet hnd . fromIntegral
        resetHandle hnd
        when (result /= testData) $ throwIO WrongData
        when shouldFail $ throwIO ShouldThrow

    case result of
        Left WrongData -> failTest "Incorrect result read from file"
        Left ShouldThrow -> return ()
        Right _ | shouldFail -> failTest "Failed to throw exception"
                | otherwise -> return ()

    existsAfter <- doesFileExist testFile
    when existsAfter $ do
        removeFile testFile
        failTest "Failed to delete file"
  where
    testFile :: String
    testFile = "tmp"

    testData :: ByteString
    testData = "blah"

    runTest :: Benchmark -> IO (Either CheckResult ())
    runTest = withArgs (["-n","1"]) . try . C.defaultMainWith config . pure
      where
        config = C.defaultConfig { verbosity = Quiet , timeLimit = 1 }

    failTest :: String -> IO ()
    failTest s = assertFailure $ s ++ " in test: " ++ name ++ "!"

    resetHandle :: Handle -> IO ()
    resetHandle hnd = hSeek hnd AbsoluteSeek 0

    alloc :: IO Handle
    alloc = do
        hnd <- openFile testFile ReadWriteMode
        BS.hPut hnd testData
        resetHandle hnd
        return hnd

    clean :: Handle -> IO ()
    clean hnd = do
        hClose hnd
        removeFile testFile

testSuccess :: String -> BenchmarkWithFile -> TestTree
testSuccess = testCleanup False

testFailure :: String -> BenchmarkWithFile -> TestTree
testFailure = testCleanup True

main :: IO ()
main = defaultMain $ testGroup "cleanup"
    [ testSuccess "perRun Success" perRun
    , testFailure "perRun Failure" perRun
    , testSuccess "perBatch Success" perBatch
    , testFailure "perBatch Failure" perBatch
    , testSuccess "envWithCleanup Success" envWithCleanup
    , testFailure "envWithCleanup Failure" envWithCleanup
    ]
