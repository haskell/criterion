{-# LANGUAGE ScopedTypeVariables #-}

module Criterion.Plot
    (
      plotWith
    , foo
    , bar
    ) where

import Criterion.Config
import Criterion.IO (printError)
import Data.Array.Vector
import Data.Char (isSpace)
import Data.Foldable (forM_)
import Data.List (group)
import Statistics.KernelDensity (Points, fromPoints)
import Graphics.Rendering.Chart.Simple hiding (plot)
import System.FilePath (addExtension, pathSeparator)
import System.IO (IOMode(..), Handle, hPutStr, stdout, withBinaryFile)
import qualified Criterion.MultiMap as M
import Graphics.Rendering.Chart hiding (Plot,c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Statistics.Types (Sample)
import Text.Printf

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

renderTimes :: String -> Sample -> Renderable ()
renderTimes desc times = toRenderable layout
  where
    layout = layout1_title ^= "Execution times for \"" ++ desc ++ "\""
           $ layout1_plots ^= [ Left (plotBars bars) ]
           $ layout1_left_axis ^= leftAxis
           $ layout1_bottom_axis ^= bottomAxis
           $ defaultLayout1 :: Layout1 Double Double

    leftAxis = laxis_generate ^= autoScaledAxis secAxis
             $ laxis_title ^= "execution time"
             $ defaultLayoutAxis

    bottomAxis = laxis_title ^= "number of samples"
               $ defaultLayoutAxis

    bars = plot_bars_values ^= (zip [0..] . map (:[]) . fromU $ times)
         $ defaultPlotBars

renderKDE :: String -> Points -> UArr Double -> Renderable ()
renderKDE desc points pdf = toRenderable layout
  where
    layout = layout1_title ^= "Densities of execution times for \"" ++
                              desc ++ "\""
           $ layout1_plots ^= [ Left (toPlot lines) ]
           $ layout1_left_axis ^= leftAxis
           $ layout1_bottom_axis ^= bottomAxis
           $ defaultLayout1 :: Layout1 Double Double

    leftAxis = laxis_title ^= "estimate of probability density"
             $ defaultLayoutAxis

    bottomAxis = laxis_generate ^= autoScaledAxis secAxis
               $ laxis_title ^= "execution time"
               $ defaultLayoutAxis

    lines = plot_lines_values ^= [zip (fromU (fromPoints points)) (fromU spdf)]
          $ defaultPlotLines

    spdf = mapU (/ sumU pdf) pdf

secAxis :: LinearAxisParams
secAxis = la_labelf ^= secs
        $ defaultLinearAxis

foo desc times = renderableToWindow (renderTimes desc times) 800 600
bar desc points pdf = renderableToWindow (renderKDE desc points pdf) 800 600

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1e9   = (k/1e9)  `with` "Gs"
    | k >= 1e6   = (k/1e6)  `with` "Ms"
    | k >= 1e4   = (k/1e3)  `with` "Ks"
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.0f %s" t u
               | t >= 1e4  = printf "%.0f %s" t u
               | t >= 1e3  = printf "%.0f %s" t u
               | t >= 1e2  = printf "%.0f %s" t u
               | t >= 1e1  = printf "%.1f %s" t u
               | otherwise = printf "%.1f %s" t u
