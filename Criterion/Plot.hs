module Criterion.Plot
    (
      plotWith
    ) where

import Criterion.Config
import Criterion.IO (printError)
import Data.Array.Vector
import Data.Char (isSpace)
import Data.Foldable (forM_)
import Data.List (group)
import Graphics.Rendering.Chart.Simple hiding (plot)
import System.FilePath (addExtension, pathSeparator)
import System.IO (IOMode(..), Handle, hPutStr, stdout, withBinaryFile)
import qualified Criterion.MultiMap as M

manglePath :: String -> String -> FilePath
manglePath _ "-"    = "-"
manglePath sfx name = (`addExtension` sfx) .
                      concatMap (replace ((==) '-' . head) "-") .
                      group .
                      map (replace isSpace '-') .
                      map (replace (==pathSeparator) '-') $
                      name
    where replace p r c | p c       = r
                        | otherwise = c

plotWith :: Plot -> Config -> String -> String -> String
         -> UArr Double -> UArr Double -> IO ()
plotWith p cfg title xlabel ylabel xdata ydata =
  case M.lookup p (cfgPlot cfg) of
    Nothing -> return ()
    Just s -> forM_ s $ \t -> plot t title xlabel ylabel xdata ydata
            
plot :: PlotOutput -> String -> String -> String
     -> UArr Double -> UArr Double -> IO ()

plot Window _title xlabel ylabel xdata ydata = do
  plotWindow xlabel (fromU xdata) ylabel (fromU ydata)

plot CSV title xlabel ylabel xdata ydata = do
  writeTo (manglePath "csv" title) $ \h -> do
    putLn h (escapeCSV xlabel ++ ',' : escapeCSV ylabel)
    forM_ (fromU $ zipU xdata ydata) $ \(x :*: y) ->
      putLn h (show x ++ ',' : show y)

plot dest _ _ _ _ _ = do
  printError "plot %s: not yet implemented\n" (show dest)

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
