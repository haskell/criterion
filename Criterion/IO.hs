{-# LANGUAGE CPP, OverloadedStrings #-}
-- |
-- Module      : Criterion.IO
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Input and output actions.

module Criterion.IO
    (
      header
    , hGetReports
    , hPutReports
    , readReports
    , writeReports
    ) where

import Criterion.Types (Report(..))
import Data.Binary (Binary(..), encode)
#if MIN_VERSION_binary(0, 6, 3)
import Data.Binary.Get (runGetOrFail)
#else
import Data.Binary.Get (runGetState)
#endif
import Data.Binary.Put (putByteString, putWord16be, runPut)
import Data.ByteString.Char8 ()
import Data.Version (Version(..))
import Paths_criterion (version)
import System.IO (Handle, IOMode(..), withFile)
import qualified Data.ByteString.Lazy as L

-- | The header identifies a criterion data file. This contains
-- version information; there is no expectation of cross-version
-- compatibility.
header :: L.ByteString
header = runPut $ do
  putByteString "criterio"
  mapM_ (putWord16be . fromIntegral) (versionBranch version)

-- | Read all reports from the given 'Handle'.
hGetReports :: Handle -> IO (Either String [Report])
hGetReports handle = do
  bs <- L.hGet handle (fromIntegral (L.length header))
  if bs == header
    then Right `fmap` readAll handle
    else return $ Left "unexpected header"

-- | Write reports to the given 'Handle'.
hPutReports :: Handle -> [Report] -> IO ()
hPutReports handle rs = do
  L.hPut handle header
  mapM_ (L.hPut handle . encode) rs

-- | Read all reports from the given file.
readReports :: FilePath -> IO (Either String [Report])
readReports path = withFile path ReadMode hGetReports

-- | Write reports to the given file.
writeReports :: FilePath -> [Report] -> IO ()
writeReports path rs = withFile path WriteMode (flip hPutReports rs)

#if MIN_VERSION_binary(0, 6, 3)
readAll :: Binary a => Handle -> IO [a]
readAll handle = do
  let go bs
         | L.null bs = return []
         | otherwise = case runGetOrFail get bs of
                         Left (_, _, err) -> fail err
                         Right (bs', _, a) -> (a:) `fmap` go bs'
  go =<< L.hGetContents handle
#else
readAll :: Binary a => Handle -> IO [a]
readAll handle = do
  let go i bs
         | L.null bs = return []
         | otherwise =
            let (a, bs', i') = runGetState get bs i
             in (a:) `fmap` go i' bs'
  go 0 =<< L.hGetContents handle
#endif
