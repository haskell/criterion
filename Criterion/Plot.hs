{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators #-}

-- |
-- Module      : Criterion.Plot
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
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

import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.IO (printError)
import Criterion.Monad (Criterion, getConfigItem)
import Data.Char (isSpace, toLower)
import Data.Foldable (forM_)
import Data.List (group, intersperse)
import Statistics.Function (indexed)
import Statistics.Types (Sample)
import System.FilePath (isPathSeparator)
import System.IO (IOMode(..), Handle, hPutStr, withBinaryFile)
import Text.Printf (printf)
import qualified Criterion.MultiMap as M
import qualified Data.Vector.Unboxed as U

#ifdef HAVE_CHART
# if MIN_VERSION_base(4,3,0)
import Control.Monad (void)
# endif

import Data.Accessor ((^=))
import Graphics.Rendering.Chart hiding (Plot,c)
# ifdef HAVE_GTK
import Graphics.Rendering.Chart.Gtk (renderableToWindow)
# endif
#endif

plotWith :: Plot -> (PlotOutput -> IO ()) -> Criterion ()
plotWith p plot = getConfigItem (M.lookup p . cfgPlot)
                    >>= maybe (return ()) (liftIO . flip forM_ plot)

-- | Plot timing data.
plotTiming :: PlotOutput        -- ^ The kind of output desired.
           -> String            -- ^ Benchmark name.
           -> Sample            -- ^ Timing data.
           -> IO ()

plotTiming CSV desc times = do
  writeTo (mangle $ printf "%s timings.csv" desc) $ \h -> do
    putRow h ["sample", "execution time"]
    forM_ (U.toList $ indexed times) $ \(x,y) ->
      putRow h [show x, show y]

#ifdef HAVE_CHART
plotTiming (PDF x y) desc times = void $
  renderableToPDFFile (renderTiming desc times) x y
                      (mangle $ printf "%s timings %dx%d.pdf" desc x y)

plotTiming (PNG x y) desc times = void $
  renderableToPNGFile (renderTiming desc times) x y
                      (mangle $ printf "%s timings %dx%d.png" desc x y)

plotTiming (SVG x y) desc times = void $
  renderableToSVGFile (renderTiming desc times) x y
                      (mangle $ printf "%s timings %dx%d.svg" desc x y)

# ifdef HAVE_GTK
plotTiming (Window x y) desc times = void $
  renderableToWindow (renderTiming desc times) x y
# endif
#endif

plotTiming output _desc _times =
  printError "ERROR: output type %s not supported on this platform\n"
             (show output)

-- | Plot kernel density estimate.
plotKDE :: PlotOutput           -- ^ The kind of output desired.
        -> String               -- ^ Benchmark name.
        -> Maybe (Double, Double) -- ^ Range of x-axis
        -> Points               -- ^ Points at which KDE was computed.
        -> U.Vector Double      -- ^ Kernel density estimates.
        -> IO ()

plotKDE CSV desc _exs points pdf = do
  writeTo (mangle $ printf "%s densities.csv" desc) $ \h -> do
    putRow h ["execution time", "probability"]
    forM_ (zip (U.toList pdf) (U.toList (fromPoints points))) $ \(x, y) ->
      putRow h [show x, show y]

#ifdef HAVE_CHART
plotKDE (PDF x y) desc exs points pdf = void $
  renderableToPDFFile (renderKDE desc exs points pdf) x y
                      (mangle $ printf "%s densities %dx%d.pdf" desc x y)

plotKDE (PNG x y) desc exs points pdf = void $
  renderableToPNGFile (renderKDE desc exs points pdf) x y
                      (mangle $ printf "%s densities %dx%d.png" desc x y)

plotKDE (SVG x y) desc exs points pdf = void $
  renderableToSVGFile (renderKDE desc exs points pdf) x y
                      (mangle $ printf "%s densities %dx%d.svg" desc x y)

# ifdef HAVE_GTK
plotKDE (Window x y) desc exs points pdf = void $
    renderableToWindow (renderKDE desc exs points pdf) x y
# endif
#endif

plotKDE output _desc _exs _points _pdf =
  printError "ERROR: output type %s not supported on this platform\n"
             (show output)

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

    bars = plot_bars_values ^= (zip [0.5,1.5..] . map (:[]) . U.toList $ times)
         $ plot_bars_item_styles ^= [ (solidFillStyle c, Nothing) | c <- defaultColorSeq ]
         $ plot_bars_spacing ^= BarsFixGap 0 2
         $ defaultPlotBars

renderKDE :: String -> Maybe (Double, Double) -> Points -> U.Vector Double
          -> Renderable ()
renderKDE desc exs points pdf = toRenderable layout
  where
    layout = layout1_title ^= "Densities of execution times for \"" ++
                              desc ++ "\""
           $ layout1_plots ^= [ Left (toPlot info) ]
           $ layout1_left_axis ^= leftAxis
           $ layout1_bottom_axis ^= bottomAxis
           $ defaultLayout1 :: Layout1 Double Double

    leftAxis = laxis_title ^= "estimate of probability density"
             $ defaultLayoutAxis

    bottomAxis = laxis_generate ^= semiAutoScaledAxis secAxis
               $ laxis_title ^= "execution time"
               $ defaultLayoutAxis

    semiAutoScaledAxis opts ps = autoScaledAxis opts (extremities ++ ps)
    extremities = maybe [] (\(lo, hi) -> [lo, hi]) exs

    info = plot_lines_values ^= [zip (U.toList (fromPoints points)) (U.toList spdf)]
         $ defaultPlotLines

    -- Normalise the PDF estimates into a semi-sane range.
    spdf = U.map (/ U.sum pdf) pdf

-- | An axis whose labels display as seconds (or fractions thereof).
secAxis :: LinearAxisParams Double
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
       . map (replace isSpace '-' . replace isPathSeparator '-' . toLower)
    where replace p r c | p c       = r
                        | otherwise = c

#if defined(HAVE_CHART) && !MIN_VERSION_base(4,3,0)
void :: (Monad m) => m a -> m ()
void f = f >> return ()
#endif
