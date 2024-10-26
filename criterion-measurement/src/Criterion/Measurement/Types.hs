{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GADTs, RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      : Criterion.Measurement.Types
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for benchmarking.
--
-- The core type is 'Benchmarkable', which admits both pure functions
-- and 'IO' actions.
--
-- For a pure function of type @a -> b@, the benchmarking harness
-- calls this function repeatedly, each time with a different 'Int64'
-- argument (the number of times to run the function in a loop), and
-- reduces the result the function returns to weak head normal form.
--
-- For an action of type @IO a@, the benchmarking harness calls the
-- action repeatedly, but does not reduce the result.


module Criterion.Measurement.Types
  (
      -- * Benchmark descriptions
      Benchmarkable(..)
    , Benchmark(..)
    -- * Measurements
    , Measured(..)
    , fromInt
    , toInt
    , fromDouble
    , toDouble
    , measureAccessors
    , measureKeys
    , measure
    , rescale
    -- * Benchmark construction
    , env
    , envWithCleanup
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup
    , toBenchmarkable
    , bench
    , bgroup
    , addPrefix
    , benchNames
    -- ** Evaluation control
    , nf
    , whnf
    , nfIO
    , whnfIO
    , nfAppIO
    , whnfAppIO
                      )
  where

import Control.DeepSeq (NFData(rnf))
import Criterion.Measurement.Types.Internal (fakeEnvironment, nf', whnf')
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Binary (Binary(..))
import Data.Data (Data)
import Data.Int (Int64)
import Data.Map (Map, fromList)
import GHC.Generics (Generic)
import Prelude ()
import Prelude.Compat
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U


-- | A pure function or impure action that can be benchmarked. The
-- 'Int64' parameter indicates the number of times to run the given
-- function or action.
data Benchmarkable = forall a . NFData a =>
    Benchmarkable
      { allocEnv :: Int64 -> IO a
      , cleanEnv :: Int64 -> a -> IO ()
      , runRepeatedly :: a -> Int64 -> IO ()
      , perRun :: Bool
      }

noop :: Monad m => a -> m ()
noop = const $ return ()
{-# INLINE noop #-}

-- | Construct a 'Benchmarkable' value from an impure action, where the 'Int64'
-- parameter indicates the number of times to run the action.
toBenchmarkable :: (Int64 -> IO ()) -> Benchmarkable
toBenchmarkable f = Benchmarkable noop (const noop) (const f) False
{-# INLINE toBenchmarkable #-}


-- | A collection of measurements made while benchmarking.
--
-- Measurements related to garbage collection are tagged with __GC__.
-- They will only be available if a benchmark is run with @\"+RTS
-- -T\"@.
--
-- __Packed storage.__ When GC statistics cannot be collected, GC
-- values will be set to huge negative values.  If a field is labeled
-- with \"__GC__\" below, use 'fromInt' and 'fromDouble' to safely
-- convert to \"real\" values.
data Measured = Measured {
      measTime               :: !Double
      -- ^ Total wall-clock time elapsed, in seconds.
    , measCpuTime            :: !Double
      -- ^ Total CPU time elapsed, in seconds.  Includes both user and
      -- kernel (system) time.
    , measCycles             :: !Int64
      -- ^ Cycles, in unspecified units that may be CPU cycles.  (On
      -- i386 and x86_64, this is measured using the @rdtsc@
      -- instruction.)
    , measIters              :: !Int64
      -- ^ Number of loop iterations measured.

    , measAllocated          :: !Int64
      -- ^ __(GC)__ Number of bytes allocated.  Access using 'fromInt'.
    , measPeakMbAllocated    :: !Int64
      -- ^ __(GC)__ Max number of megabytes allocated.  Access using 'fromInt'.
    , measNumGcs             :: !Int64
      -- ^ __(GC)__ Number of garbage collections performed.  Access
      -- using 'fromInt'.
    , measBytesCopied        :: !Int64
      -- ^ __(GC)__ Number of bytes copied during garbage collection.
      -- Access using 'fromInt'.
    , measMutatorWallSeconds :: !Double
      -- ^ __(GC)__ Wall-clock time spent doing real work
      -- (\"mutation\"), as distinct from garbage collection.  Access
      -- using 'fromDouble'.
    , measMutatorCpuSeconds  :: !Double
      -- ^ __(GC)__ CPU time spent doing real work (\"mutation\"), as
      -- distinct from garbage collection.  Access using 'fromDouble'.
    , measGcWallSeconds      :: !Double
      -- ^ __(GC)__ Wall-clock time spent doing garbage collection.
      -- Access using 'fromDouble'.
    , measGcCpuSeconds       :: !Double
      -- ^ __(GC)__ CPU time spent doing garbage collection.  Access
      -- using 'fromDouble'.
    } deriving (Eq, Read, Show, Data, Generic)

instance FromJSON Measured where
    parseJSON v = do
      (a,b,c,d,e,f,g,h,i,j,k,l) <- parseJSON v
      -- The first four fields are not subject to the encoding policy:
      return $ Measured a b c d
                       (int e) (int f) (int g) (int h)
                       (db i) (db j) (db k) (db l)
      where int = toInt; db = toDouble

-- Here we treat the numeric fields as `Maybe Int64` and `Maybe Double`
-- and we use a specific policy for deciding when they should be Nothing,
-- which becomes null in JSON.
instance ToJSON Measured where
    toJSON Measured{..} = toJSON
      (measTime, measCpuTime, measCycles, measIters,
       i measAllocated, i measPeakMbAllocated, i measNumGcs, i measBytesCopied,
       d measMutatorWallSeconds, d measMutatorCpuSeconds,
       d measGcWallSeconds, d measGcCpuSeconds)
      where i = fromInt; d = fromDouble

instance NFData Measured where
    rnf Measured{} = ()

-- THIS MUST REFLECT THE ORDER OF FIELDS IN THE DATA TYPE.
--
-- The ordering is used by Javascript code to pick out the correct
-- index into the vector that represents a Measured value in that
-- world.
measureAccessors_ :: [(String, (Measured -> Maybe Double, String))]
measureAccessors_ = [
    ("time",               (Just . measTime,
                            "wall-clock time"))
  , ("cpuTime",            (Just . measCpuTime,
                            "CPU time"))
  , ("cycles",             (Just . fromIntegral . measCycles,
                            "CPU cycles"))
  , ("iters",              (Just . fromIntegral . measIters,
                            "loop iterations"))
  , ("allocated",          (fmap fromIntegral . fromInt . measAllocated,
                            "(+RTS -T) bytes allocated"))
  , ("peakMbAllocated",    (fmap fromIntegral . fromInt . measPeakMbAllocated,
                            "(+RTS -T) peak megabytes allocated"))
  , ("numGcs",             (fmap fromIntegral . fromInt . measNumGcs,
                            "(+RTS -T) number of garbage collections"))
  , ("bytesCopied",        (fmap fromIntegral . fromInt . measBytesCopied,
                            "(+RTS -T) number of bytes copied during GC"))
  , ("mutatorWallSeconds", (fromDouble . measMutatorWallSeconds,
                            "(+RTS -T) wall-clock time for mutator threads"))
  , ("mutatorCpuSeconds",  (fromDouble . measMutatorCpuSeconds,
                            "(+RTS -T) CPU time spent running mutator threads"))
  , ("gcWallSeconds",      (fromDouble . measGcWallSeconds,
                            "(+RTS -T) wall-clock time spent doing GC"))
  , ("gcCpuSeconds",       (fromDouble . measGcCpuSeconds,
                            "(+RTS -T) CPU time spent doing GC"))
  ]


-- | Field names in a 'Measured' record, in the order in which they
-- appear.
measureKeys :: [String]
measureKeys = map fst measureAccessors_

-- | Field names and accessors for a 'Measured' record.
measureAccessors :: Map String (Measured -> Maybe Double, String)
measureAccessors = fromList measureAccessors_

-- | Normalise every measurement as if 'measIters' was 1.
--
-- ('measIters' itself is left unaffected.)
rescale :: Measured -> Measured
rescale m@Measured{..} = m {
      measTime               = d measTime
    , measCpuTime            = d measCpuTime
    , measCycles             = i measCycles
    -- skip measIters
    , measNumGcs             = i measNumGcs
    , measBytesCopied        = i measBytesCopied
    , measMutatorWallSeconds = d measMutatorWallSeconds
    , measMutatorCpuSeconds  = d measMutatorCpuSeconds
    , measGcWallSeconds      = d measGcWallSeconds
    , measGcCpuSeconds       = d measGcCpuSeconds
    } where
        d k = maybe k (/ iters) (fromDouble k)
        i k = maybe k (round . (/ iters)) (fromIntegral <$> fromInt k)
        iters               = fromIntegral measIters :: Double

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromInt :: Int64 -> Maybe Int64
fromInt i | i == minBound = Nothing
          | otherwise     = Just i

-- | Convert from a true value back to the packed representation used
-- for GC measurements.
toInt :: Maybe Int64 -> Int64
toInt Nothing  = minBound
toInt (Just i) = i

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromDouble :: Double -> Maybe Double
fromDouble d | isInfinite d || isNaN d = Nothing
             | otherwise               = Just d

-- | Convert from a true value back to the packed representation used
-- for GC measurements.
toDouble :: Maybe Double -> Double
toDouble Nothing  = -1/0
toDouble (Just d) = d


instance Binary Measured where
    put Measured{..} = do
      put measTime; put measCpuTime; put measCycles; put measIters
      put measAllocated; put measPeakMbAllocated; put measNumGcs; put measBytesCopied
      put measMutatorWallSeconds; put measMutatorCpuSeconds
      put measGcWallSeconds; put measGcCpuSeconds
    get = Measured <$> get <*> get <*> get <*> get <*> get
                   <*> get <*> get <*> get <*> get <*> get <*> get <*> get

-- | Apply an argument to a function, and evaluate the result to
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf f x = toBenchmarkable (nf' rnf f x)

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable
whnf f x = toBenchmarkable (whnf' f x)

-- | Perform an action, then evaluate its result to normal form (NF).
-- This is particularly useful for forcing a lazy 'IO' action to be
-- completely performed.
--
-- If the construction of the 'IO a' value is an important factor
-- in the benchmark, it is best to use 'nfAppIO' instead.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO a = toBenchmarkable (nfIO' rnf a)

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).  This is useful for forcing an 'IO' action whose result
-- is an expression to be evaluated down to a more useful value.
--
-- If the construction of the 'IO a' value is an important factor
-- in the benchmark, it is best to use 'whnfAppIO' instead.
whnfIO :: IO a -> Benchmarkable
whnfIO a = toBenchmarkable (whnfIO' a)

-- | Apply an argument to a function which performs an action, then
-- evaluate its result to normal form (NF).
-- This function constructs the 'IO b' value on each iteration,
-- similar to 'nf'.
-- This is particularly useful for 'IO' actions where the bulk of the
-- work is not bound by IO, but by pure computations that may
-- optimize away if the argument is known statically, as in 'nfIO'.

-- See issue #189 for more info.
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO f v = toBenchmarkable (nfAppIO' rnf f v)

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).
-- This function constructs the 'IO b' value on each iteration,
-- similar to 'whnf'.
-- This is particularly useful for 'IO' actions where the bulk of the
-- work is not bound by IO, but by pure computations that may
-- optimize away if the argument is known statically, as in 'nfIO'.

-- See issue #189 for more info.
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO f v = toBenchmarkable (whnfAppIO' f v)

-- Along with nf' and whnf', the following two functions are the core
-- benchmarking loops. They have been carefully constructed to avoid
-- allocation while also evaluating @a@.
--
-- These functions must not be inlined. There are two possible issues that
-- can arise if they are inlined. First, the work is often floated out of
-- the loop, which creates a nonsense benchmark. Second, the benchmark code
-- itself could be changed by the user's optimization level. By marking them
-- @NOINLINE@, the core benchmark code is always the same.
--
-- See #183 and #184 for discussion.

-- | Generate a function that will run an action a given number of times,
-- reducing it to normal form each time.
nfIO' :: (a -> ()) -> IO a -> (Int64 -> IO ())
nfIO' reduce a = go
  where go n
          | n <= 0    = return ()
          | otherwise = do
              x <- a
              reduce x `seq` go (n-1)
{-# NOINLINE nfIO' #-}

-- | Generate a function that will run an action a given number of times.
whnfIO' :: IO a -> (Int64 -> IO ())
whnfIO' a = go
  where
    go n | n <= 0    = return ()
         | otherwise = do
             x <- a
             x `seq` go (n-1)
{-# NOINLINE whnfIO' #-}

-- | Generate a function which applies an argument to a function a given
-- number of times, running its action and reducing the result to normal form.
nfAppIO' :: (b -> ()) -> (a -> IO b) -> a -> (Int64 -> IO ())
nfAppIO' reduce f v = go
  where go n
          | n <= 0    = return ()
          | otherwise = do
              x <- f v
              reduce x `seq` go (n-1)
{-# NOINLINE nfAppIO' #-}

-- | Generate a function which applies an argument to a function a given
-- number of times, running its action and reducing the result to
-- weak-head normal form.
whnfAppIO' :: (a -> IO b) -> a -> (Int64 -> IO ())
whnfAppIO' f v = go
  where go n
          | n <= 0    = return ()
          | otherwise = do
              x <- f v
              x `seq` go (n-1)
{-# NOINLINE whnfAppIO' #-}

-- | Specification of a collection of benchmarks and environments. A
-- benchmark may consist of:
--
-- * An environment that creates input data for benchmarks, created
--   with 'env'.
--
-- * A single 'Benchmarkable' item with a name, created with 'bench'.
--
-- * A (possibly nested) group of 'Benchmark's, created with 'bgroup'.
data Benchmark where
    Environment  :: NFData env
                 => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
    Benchmark    :: String -> Benchmarkable -> Benchmark
    BenchGroup   :: String -> [Benchmark] -> Benchmark


-- | Run a benchmark (or collection of benchmarks) in the given
-- environment.  The purpose of an environment is to lazily create
-- input data to pass to the functions that will be benchmarked.
--
-- A common example of environment data is input that is read from a
-- file.  Another is a large data structure constructed in-place.
--
-- __Motivation.__ In earlier versions of criterion, all benchmark
-- inputs were always created when a program started running.  By
-- deferring the creation of an environment when its associated
-- benchmarks need the its, we avoid two problems that this strategy
-- caused:
--
-- * Memory pressure distorted the results of unrelated benchmarks.
--   If one benchmark needed e.g. a gigabyte-sized input, it would
--   force the garbage collector to do extra work when running some
--   other benchmark that had no use for that input.  Since the data
--   created by an environment is only available when it is in scope,
--   it should be garbage collected before other benchmarks are run.
--
-- * The time cost of generating all needed inputs could be
--   significant in cases where no inputs (or just a few) were really
--   needed.  This occurred often, for instance when just one out of a
--   large suite of benchmarks was run, or when a user would list the
--   collection of benchmarks without running any.
--
-- __Creation.__ An environment is created right before its related
-- benchmarks are run.  The 'IO' action that creates the environment
-- is run, then the newly created environment is evaluated to normal
-- form (hence the 'NFData' constraint) before being passed to the
-- function that receives the environment.
--
-- __Complex environments.__ If you need to create an environment that
-- contains multiple values, simply pack the values into a tuple.
--
-- __Lazy pattern matching.__ In situations where a \"real\"
-- environment is not needed, e.g. if a list of benchmark names is
-- being generated, a value which throws an exception will be passed
-- to the function that receives the environment.  This avoids the
-- overhead of generating an environment that will not actually be
-- used.
--
-- The function that receives the environment must use lazy pattern
-- matching to deconstruct the tuple (e.g., @~(x, y)@, not @(x, y)@),
-- as use of strict pattern matching will cause a crash if an
-- exception-throwing value is passed in.
--
-- __Example.__ This program runs benchmarks in an environment that
-- contains two values.  The first value is the contents of a text
-- file; the second is a string.  Pay attention to the use of a lazy
-- pattern to deconstruct the tuple in the function that returns the
-- benchmarks to be run.
--
-- > setupEnv = do
-- >   let small = replicate 1000 (1 :: Int)
-- >   big <- map length . words <$> readFile "/usr/dict/words"
-- >   return (small, big)
-- >
-- > main = defaultMain [
-- >    -- notice the lazy pattern match here!
-- >    env setupEnv $ \ ~(small,big) -> bgroup "main" [
-- >    bgroup "small" [
-- >      bench "length" $ whnf length small
-- >    , bench "length . filter" $ whnf (length . filter (==1)) small
-- >    ]
-- >  ,  bgroup "big" [
-- >      bench "length" $ whnf length big
-- >    , bench "length . filter" $ whnf (length . filter (==1)) big
-- >    ]
-- >  ] ]
--
-- __Discussion.__ The environment created in the example above is
-- intentionally /not/ ideal.  As Haskell's scoping rules suggest, the
-- variable @big@ is in scope for the benchmarks that use only
-- @small@.  It would be better to create a separate environment for
-- @big@, so that it will not be kept alive while the unrelated
-- benchmarks are being run.
env :: NFData env =>
       IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
env alloc = Environment alloc noop

-- | Same as `env`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
envWithCleanup
    :: NFData env
    => IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> IO a)
    -- ^ Clean up the created environment.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
envWithCleanup = Environment

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- batch of runs of the benchmarkable.
--
-- The environment is evaluated to normal form before the benchmark is run.
--
-- When using 'whnf', 'whnfIO', etc. Criterion creates a 'Benchmarkable'
-- whichs runs a batch of @N@ repeat runs of that expressions. Criterion may
-- run any number of these batches to get accurate measurements. Environments
-- created by 'env' and 'envWithCleanup', are shared across all these batches
-- of runs.
--
-- This is fine for simple benchmarks on static input, but when benchmarking
-- IO operations where these operations can modify (and especially grow) the
-- environment this means that later batches might have their accuracy effected
-- due to longer, for example, longer garbage collection pauses.
--
-- An example: Suppose we want to benchmark writing to a Chan, if we allocate
-- the Chan using environment and our benchmark consists of @writeChan env ()@,
-- the contents and thus size of the Chan will grow with every repeat. If
-- Criterion runs a 1,000 batches of 1,000 repeats, the result is that the
-- channel will have 999,000 items in it by the time the last batch is run.
-- Since GHC GC has to copy the live set for every major GC this means our last
-- set of writes will suffer a lot of noise of the previous repeats.
--
-- By allocating a fresh environment for every batch of runs this function
-- should eliminate this effect.
perBatchEnv
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnv alloc = perBatchEnvWithCleanup alloc (const noop)

-- | Same as `perBatchEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perBatchEnvWithCleanup
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (Int64 -> env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnvWithCleanup alloc clean work
    = Benchmarkable alloc clean (nfIO' rnf . work) False

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- run of the operation to benchmark. This is useful for benchmarking mutable
-- operations that need a fresh environment, such as sorting a mutable Vector.
--
-- As with 'env' and 'perBatchEnv' the environment is evaluated to normal form
-- before the benchmark is run.
--
-- This introduces extra noise and result in reduce accuracy compared to other
-- Criterion benchmarks. But allows easier benchmarking for mutable operations
-- than was previously possible.
perRunEnv
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perRunEnv alloc = perRunEnvWithCleanup alloc noop

-- | Same as `perRunEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perRunEnvWithCleanup
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perRunEnvWithCleanup alloc clean work = bm { perRun = True }
  where
    bm = perBatchEnvWithCleanup (const alloc) (const clean) work

-- | Create a single benchmark.
bench :: String                 -- ^ A name to identify the benchmark.
      -> Benchmarkable          -- ^ An activity to be benchmarked.
      -> Benchmark
bench = Benchmark

-- | Group several benchmarks together under a common name.
bgroup :: String                -- ^ A name to identify the group of benchmarks.
       -> [Benchmark]           -- ^ Benchmarks to group under this name.
       -> Benchmark
bgroup = BenchGroup

-- | Add the given prefix to a name.  If the prefix is empty, the name
-- is returned unmodified.  Otherwise, the prefix and name are
-- separated by a @\'\/\'@ character.
addPrefix :: String             -- ^ Prefix.
          -> String             -- ^ Name.
          -> String
addPrefix ""  desc = desc
addPrefix pfx desc = pfx ++ '/' : desc

-- | Retrieve the names of all benchmarks.  Grouped benchmarks are
-- prefixed with the name of the group they're in.
benchNames :: Benchmark -> [String]
benchNames (Environment _ _ b) = benchNames (b fakeEnvironment)
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = map (addPrefix d) . concatMap benchNames $ bs

instance Show Benchmark where
    show (Environment _ _ b) = "Environment _ _" ++ show (b fakeEnvironment)
    show (Benchmark d _)   = "Benchmark " ++ show d
    show (BenchGroup d _)  = "BenchGroup " ++ show d

measure :: (U.Unbox a) => (Measured -> a) -> V.Vector Measured -> U.Vector a
measure f v = U.convert . V.map f $ v
