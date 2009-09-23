module Criterion.Plot
    (
      PlotType(..)
    , plot
    ) where

import Control.Monad
import Criterion.Config
import Data.Char
import Data.List
import Graphics.Rendering.Chart.Simple hiding (plot)
import Data.Array.Vector
import System.IO
import System.FilePath

mangle :: String -> String -> FilePath
mangle _ "-"    = "-"
mangle sfx name = (`addExtension` sfx) .
                  concatMap (replace ((==) '-' . head) "-") .
                  group .
                  map (replace isSpace '-') .
                  map (replace (==pathSeparator) '-') $
                  name
    where replace p r c | p c       = r
                        | otherwise = c

plot :: PlotType -> String -> String -> String
     -> UArr Double -> UArr Double -> IO ()

plot Window _title xlabel ylabel xdata ydata = do
  plotWindow xlabel (fromU xdata) ylabel (fromU ydata)

plot CSV title xlabel ylabel xdata ydata = do
  writeTo (mangle "csv" title) $ \h -> do
    putLn h (escapeCSV xlabel ++ ',' : escapeCSV ylabel)
    forM_ (fromU $ zipU xdata ydata) $ \(x :*: y) ->
      putLn h (show x ++ ',' : show y)

plot dest _ _ _ _ _ = do
  hPutStrLn stderr $ "plot " ++ show dest ++ ": not yet implemented"

writeTo :: FilePath -> (Handle -> IO a) -> IO a
writeTo "-" act  = act stdout
writeTo path act = withBinaryFile path WriteMode act

escapeCSV :: String -> String
escapeCSV xs | any (`elem`xs) escapes = '"' : concatMap esc xs ++ "\""
          | otherwise              = xs
    where esc '"' = "\"\""
          esc c   = [c]
          escapes = "\"\r\n,"

putLn :: Handle -> String -> IO ()
putLn h s = hPutStr h (s ++ "\r\n")
