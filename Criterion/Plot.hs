{-# LANGUAGE ScopedTypeVariables #-}

module Criterion.Plot
    (
      plotKDE
    , plotTiming
    , plotWith
    ) where

import Criterion.Config
import Data.Accessor ((^=))
import Data.Array.Vector
import Data.Char (isSpace)
import Data.Foldable (forM_)
import Data.List (group)
import Graphics.Rendering.Chart hiding (Plot,c)
import Graphics.Rendering.Chart.Gtk (renderableToWindow)
import Statistics.KernelDensity (Points, fromPoints)
import Statistics.Types (Sample)
import System.FilePath (addExtension, pathSeparator)
import System.IO (IOMode(..), Handle, hPutStr, stdout, withBinaryFile)
import Text.Printf (printf)
import qualified Criterion.MultiMap as M

plotWith :: Plot -> Config -> (PlotOutput -> IO ()) -> IO ()
plotWith p cfg plot =
  case M.lookup p (cfgPlot cfg) of
    Nothing -> return ()
    Just s -> forM_ s $ plot

plotTiming :: PlotOutput -> String -> Sample -> IO ()

plotTiming CSV desc times = do
  writeTo (manglePath "csv" (desc ++ " timings")) $ \h -> do
    putLn h (escapeCSV "sample" ++ ',' : escapeCSV "execution time")
    forM_ (fromU $ indexedU times) $ \(x :*: y) ->
      putLn h (show x ++ ',' : show y)

plotTiming (PDF x y) desc times =
  renderableToPDFFile (renderTiming desc times) x y
                      (manglePath "pdf" $ printf "%s timings %dx%d" desc x y)

plotTiming (PNG x y) desc times =
  renderableToPNGFile (renderTiming desc times) x y
                      (manglePath "png" $ printf "%s timings %dx%d" desc x y)

plotTiming (SVG x y) desc times =
  renderableToSVGFile (renderTiming desc times) x y
                      (manglePath "svg" $ printf "%s timings %dx%d" desc x y)

plotTiming (Window x y) desc times =
  renderableToWindow (renderTiming desc times) x y

plotKDE :: PlotOutput -> String -> Points -> UArr Double -> IO ()

plotKDE CSV desc points pdf = do
  writeTo (manglePath "csv" (desc ++ " densities")) $ \h -> do
    putLn h (escapeCSV "execution time" ++ ',' : escapeCSV "probability")
    forM_ (zip (fromU pdf) (fromU (fromPoints points))) $ \(x, y) ->
      putLn h (show x ++ ',' : show y)

plotKDE (PDF x y) desc points pdf =
  renderableToPDFFile (renderKDE desc points pdf) x y
                      (manglePath "pdf" $ printf "%s densities %dx%d" desc x y)

plotKDE (PNG x y) desc points pdf =
  renderableToPNGFile (renderKDE desc points pdf) x y
                      (manglePath "png" $ printf "%s densities %dx%d" desc x y)

plotKDE (SVG x y) desc points pdf =
  renderableToSVGFile (renderKDE desc points pdf) x y
                      (manglePath "svg" $ printf "%s densities %dx%d" desc x y)

plotKDE (Window x y) desc points pdf =
    renderableToWindow (renderKDE desc points pdf) x y

renderTiming :: String -> Sample -> Renderable ()
renderTiming desc times = toRenderable layout
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
           $ layout1_plots ^= [ Left (toPlot info) ]
           $ layout1_left_axis ^= leftAxis
           $ layout1_bottom_axis ^= bottomAxis
           $ defaultLayout1 :: Layout1 Double Double

    leftAxis = laxis_title ^= "estimate of probability density"
             $ defaultLayoutAxis

    bottomAxis = laxis_generate ^= autoScaledAxis secAxis
               $ laxis_title ^= "execution time"
               $ defaultLayoutAxis

    info = plot_lines_values ^= [zip (fromU (fromPoints points)) (fromU spdf)]
         $ defaultPlotLines

    spdf = mapU (/ sumU pdf) pdf

secAxis :: LinearAxisParams
secAxis = la_labelf ^= secs
        $ defaultLinearAxis

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
               | otherwise = printf "%.2f %s" t u
