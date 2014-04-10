{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Criterion.IO
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
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
    , hGetResults
    , hPutResults
    , readResults
    , writeResults
    ) where

import Criterion.Types (ResultForest, ResultTree(..))
import Data.Binary (Binary(..), encode)
#if MIN_VERSION_binary(0,6,3)
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

header :: L.ByteString
header = runPut $ do
  putByteString "criterio"
  mapM_ (putWord16be . fromIntegral) (versionBranch version)

hGetResults :: Handle -> IO (Either String ResultForest)
hGetResults handle = do
  let fixup = reverse . nukem . reverse
      nukem (Compare k _ : rs) = let (cs, rs') = splitAt k rs
                                 in Compare k (fixup (reverse cs)) : nukem rs'
      nukem (r : rs)           = r : nukem rs
      nukem _                  = []
  bs <- L.hGet handle (fromIntegral (L.length header))
  if bs == header
    then (Right . fixup) `fmap` readAll handle
    else return $ Left "unexpected header"

hPutResults :: Handle -> ResultForest -> IO ()
hPutResults handle rs = do
  L.hPut handle header
  mapM_ (L.hPut handle . encode) rs

readResults :: FilePath -> IO (Either String ResultForest)
readResults path = withFile path ReadMode hGetResults

writeResults :: FilePath -> ResultForest -> IO ()
writeResults path rs = withFile path WriteMode (flip hPutResults rs)

readAll :: Binary a => Handle -> IO [a]
#if MIN_VERSION_binary(0,6,3)
readAll handle = do
  let go bs
         | L.null bs = return []
         | otherwise = case runGetOrFail get bs of
                         Left (_, _, err) -> fail err
                         Right (bs', _, a) -> (a:) `fmap` go bs'
  go =<< L.hGetContents handle
#else
readAll handle = do
  let go i bs
         | L.null bs = return []
         | otherwise =
            let (a, bs', i') = runGetState get bs i
             in (a:) `fmap` go i' bs'
  go 0 =<< L.hGetContents handle
#endif
