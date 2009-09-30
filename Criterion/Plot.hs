{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |
-- Module      : Criterion.Plot
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Plotting functions.

module Criterion.Plot
    (
      plotKDE
    , plotTiming
    , plotWith
    ) where

import Criterion.Config
import Data.Array.Vector
import Data.Char (isSpace, toLower)
import Data.Foldable (forM_)
import Data.List (group, intersperse)
import Statistics.KernelDensity (Points, fromPoints)
import Statistics.Types (Sample)
import System.FilePath (pathSeparator)
import System.IO (IOMode(..), Handle, hPutStr, withBinaryFile)
import Text.Printf (printf)
import qualified Criterion.MultiMap as M

#ifdef HAVE_CHART
import Data.Accessor ((^=))
import Graphics.Rendering.Chart hiding (Plot,c)
import Graphics.Rendering.Chart.Gtk (renderableToWindow)
#else
import Criterion.IO (printError)
#endif

plotWith :: Plot -> Config -> (PlotOutput -> IO ()) -> IO ()
plotWith p cfg plot =
  case M.lookup p (cfgPlot cfg) of
    Nothing -> return ()
    Just s -> forM_ s $ plot

-- | Plot timing data.
plotTiming :: PlotOutput        -- ^ The kind of output desired.
           -> String            -- ^ Benchmark name.
           -> Sample            -- ^ Timing data.
           -> IO ()

plotTiming CSV desc times = do
  writeTo (mangle $ printf "%s timings.csv" desc) $ \h -> do
    putRow h ["sample", "execution time"]
    forM_ (fromU $ indexedU times) $ \(x :*: y) ->
      putRow h [show x, show y]

#ifdef HAVE_CHART
plotTiming (PDF x y) desc times =
  renderableToPDFFile (renderTiming desc times) x y
                      (mangle $ printf "%s timings %dx%d.pdf" desc x y)

plotTiming (PNG x y) desc times =
  renderableToPNGFile (renderTiming desc times) x y
                      (mangle $ printf "%s timings %dx%d.png" desc x y)

plotTiming (SVG x y) desc times =
  renderableToSVGFile (renderTiming desc times) x y
                      (mangle $ printf "%s timings %dx%d.svg" desc x y)

plotTiming (Window x y) desc times =
  renderableToWindow (renderTiming desc times) x y
#else
plotTiming output _desc _times =
  printError "ERROR: output type %s not supported on this platform\n"
             (show output)
#endif

-- | Plot kernel density estimate.
plotKDE :: PlotOutput           -- ^ The kind of output desired.
        -> String               -- ^ Benchmark name.
        -> Points               -- ^ Points at which KDE was computed.
        -> UArr Double          -- ^ Kernel density estimates.
        -> IO ()

plotKDE CSV desc points pdf = do
  writeTo (mangle $ printf "%s densities.csv" desc) $ \h -> do
    putRow h ["execution time", "probability"]
    forM_ (zip (fromU pdf) (fromU (fromPoints points))) $ \(x, y) ->
      putRow h [show x, show y]

#ifdef HAVE_CHART
plotKDE (PDF x y) desc points pdf =
  renderableToPDFFile (renderKDE desc points pdf) x y
                      (mangle $ printf "%s densities %dx%d.pdf" desc x y)

plotKDE (PNG x y) desc points pdf =
  renderableToPNGFile (renderKDE desc points pdf) x y
                      (mangle $ printf "%s densities %dx%d.png" desc x y)

plotKDE (SVG x y) desc points pdf =
  renderableToSVGFile (renderKDE desc points pdf) x y
                      (mangle $ printf "%s densities %dx%d.svg" desc x y)

plotKDE (Window x y) desc points pdf =
    renderableToWindow (renderKDE desc points pdf) x y
#else
plotKDE output _desc _points _pdf =
  printError "ERROR: output type %s not supported on this platform\n"
             (show output)
#endif

#ifdef HAVE_CHART
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

    -- Normalise the PDF estimates into a semi-sane range.
    spdf = mapU (/ sumU pdf) pdf

-- | An axis whose labels display as seconds (or fractions thereof).
secAxis :: LinearAxisParams
secAxis = la_labelf ^= secs
        $ defaultLinearAxis

-- | Try to render meaningful time-axis labels.
--
-- /FIXME/: Trouble is, we need to know the range of times for this to
-- work properly, so that we don't accidentally display consecutive
-- values that appear identical (e.g. \"43 ms, 43 ms\").
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
#endif

writeTo :: FilePath -> (Handle -> IO a) -> IO a
writeTo path = withBinaryFile path WriteMode

escapeCSV :: String -> String
escapeCSV xs | any (`elem`xs) escapes = '"' : concatMap esc xs ++ "\""
          | otherwise              = xs
    where esc '"' = "\"\""
          esc c   = [c]
          escapes = "\"\r\n,"

putRow :: Handle -> [String] -> IO ()
putRow h s = hPutStr h (concat (intersperse "," (map escapeCSV s)) ++ "\r\n")

-- | Get rid of spaces and other potentially troublesome characters
-- from output.
mangle :: String -> FilePath
mangle = concatMap (replace ((==) '-' . head) "-")
       . group
       . map (replace isSpace '-' . replace (==pathSeparator) '-' . toLower)
    where replace p r c | p c       = r
                        | otherwise = c
