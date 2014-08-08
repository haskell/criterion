% A criterion tutorial
% Learn how to write Haskell microbenchmarks.

# Installation

To install the `criterion` package, simply use `cabal`, the standard Haskell
package management command.

~~~~
cabal update
cabal install -j --disable-tests criterion
~~~~

Depending on how many prerequisites you already have installed, and
what your Cabal configuration looks like, the build may take a few
minutes: a few seconds for `criterion`, and the rest for its
dependencies.


# Getting started

Here's a a simple and complete benchmark, measuring the performance of
the ever-ridiculous `fib` function.

~~~~ {.haskell}
import Criterion.Main

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

-- Our benchmark harness.
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]
~~~~
([examples/Fibber.hs](https://github.com/bos/criterion/blob/master/examples/Fibber.hs))

The
[`defaultMain`](http://hackage.haskell.org/package/criterion/docs/Criterion-Main.html#v:defaultMain)
function takes a list of
[`Benchmark`](http://hackage.haskell.org/package/criterion/docs/Criterion-Main.html#t:Benchmark)
values, each of which describes a function to benchmark.  (We'll come
back to `bench` and `whnf` shortly, don't worry.)

To maximise our convenience, `defaultMain` will parse command line
arguments and then run any benchmarks we ask.  Let's compile our
benchmark program.

~~~~
$ ghc -O --make Fibber
[1 of 1] Compiling Main             ( Fibber.hs, Fibber.o )
Linking Fibber ...
~~~~

If we run our newly compiled `Fibber` program, it will benchmark all
of the functions we specified.

~~~~
$ ./Fibber --output fibber.html
benchmarking fib/1
time                 23.91 ns   (23.30 ns .. 24.54 ns)
                     0.994 R²   (0.991 R² .. 0.997 R²)
mean                 24.36 ns   (23.77 ns .. 24.97 ns)
std dev              2.033 ns   (1.699 ns .. 2.470 ns)
variance introduced by outliers: 88% (severely inflated)

...more output follows...
~~~~

Even better, the `--output` option directs our program to write a
report to the file [`fibber.html`](fibber.html).  Click on the image
to see a complete report.  If you mouse over the data points in the
charts, you'll see that they are *live*, giving additional information
about what's being displayed.

<a href="fibber.html" target="_blank"><img src="fibber-screenshot.png"></img></a>


# Understanding charts

A report begins with a summary of all the numbers measured.
Underneath is a breakdown of every benchmark, each with two charts and
some explanation.

The chart on the left is a
[kernel density estimate](https://en.wikipedia.org/wiki/Kernel_density_estimation)
(also known as a KDE) of time measurements.  This graphs the
*probability* of any given time measurement occurring.  A spike
indicates that a measurement of a particular time occurred; its height
indicates how often that measurement was repeated.

<div class="bs-callout bs-callout-info">
#### Why not use a histogram?

A more popular alternative to the KDE for this kind of display is the
[histogram](https://en.wikipedia.org/wiki/Histogram).  Why do we use a
KDE instead?  In order to get good information out of a histogram, you
have to
[choose a suitable bin size](https://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width).
This is a fiddly manual task.  In contrast, a KDE is likely to be
informative immediately, with no configuration required.
</div>

The chart on the right contains the raw measurements from which the
kernel density estimate was built. The $x$ axis indicates the number
of loop iterations, while the $y$ axis shows measured execution time
for the given number of iterations. The line "behind" the values is a
linear regression generated from this data.  Ideally, all measurements
will be on (or very near) this line.

## Understanding the data under a chart

Underneath the chart for each benchmark is a small table of
information that looks like this.

<table>
  <thead>
   <tr><th></th>
   <th style="opacity:0.6;font-weight:500;" title="0.95 confidence level">lower bound</th>
   <th style="font-weight:500">estimate</th>
   <th style="opacity:0.6;font-weight:500;" title="0.95 confidence level">upper bound</th>
  </tr></thead>
  <tbody>
   <tr>
    <td title="Estimate of expected time for a single execution.">OLS regression</td>
    <td title="95% of estimates fall above this value."><span style="opacity:0.4">31.0 ms</span></td>
    <td title="Estimate of expected execution time.">37.4 ms</td>
    <td title="95% of estimates fall below this value."><span style="opacity:0.4">42.9 ms</span></td>
   </tr>
   <tr>
    <td title="Numeric description of the how well the OLS estimate above fits the actual data.">R² goodness-of-fit</td>
    <td title="95% of estimates fall above this value. Note that this lower bound is suspiciously low, as it is less than 0.9."><span style="opacity:0.4">0.887</span></td>
    <td title="This value is between 0 and 1. A value below 0.99 indicates a somewhat poor fit. Values below 0.9 are outright suspicious.">0.942</td>
    <td title="95% of estimates fall below this value."><span style="opacity:0.4">0.994</span></td>
   </tr>
   <tr>
    <td>Mean execution time</td>
    <td title="95% of estimates fall above this value."><span style="opacity:0.4">34.8 ms</span></td>
    <td title="The estimated mean execution time.">37.0 ms</td>
    <td title="95% of estimates fall below this value."><span style="opacity:0.4">43.1 ms</span></td>
   </tr>
   <tr>
    <td>Standard deviation</td>
    <td title="95% of estimates fall above this value."><span style="opacity:0.4">2.11 ms</span></td>
    <td title="The estimated standard deviation of execution time.">6.49 ms</td>
    <td title="95% of estimates fall below this value."><span style="opacity:0.4">11.0 ms</span></td>
   </tr>
  </tbody>
 </table>

The first two rows are the results of a linear regression run on the measurements displayed in the right-hand chart.

* "**OLS regression**" estimates the time needed for a single
  execution of the activity being benchmarked, using an
  [ordinary least-squares regression model](https://en.wikipedia.org/wiki/Ordinary_least_squares).
  This number should be similar to the "mean estimate" row beneath it.
  It is usually more accurate, as it more effectively eliminates
  measurement overhead and other constant factors.

* "**R² goodness-of-fit**" is a measure of how accurately the linear
  regression model fits the observed measurements. If the measurements
  are not too noisy, R² should lie between 0.99 and 1, indicating an
  excellent fit. If the number is below 0.99, something is confounding
  the accuracy of the linear model.  A value below 0.9 is outright
  worrisome.

* "**Mean execution time**" and "**standard deviation**" are
  statistics calculated (more or less) from execution time divided by
  number of iterations.

On either side of the main column of values are greyed-out lower and
upper bounds.  These measure the *accuracy* of the main estimate using
a statistical technique called
[*bootstrapping*](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)). This
tells us that when randomly resampling the data, 95% of estimates fell
within between the lower and upper bounds.  When the main estimate is
of good quality, the lower and upper bounds will be close to its
value.
