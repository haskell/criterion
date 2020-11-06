Unreleased

* The HTML reports have been reworked.

  * The `flot` plotting library (`js-flot` on Hackage) has been replaced by
    `Chart.js` (`js-chart`).
  * Most practical changes focus on improving the functionality of the overview
    chart:
    * It now supports logarithmic scale (#213). The scale can be toggled by
      clicking the x-axis.
    * Manual zooming has been replaced by clicking to focus a single bar.
    * It now supports a variety of sort orders.
    * The legend can now be toggled on/off and is hidden by default.
    * Clicking the name of a group in the legend shows/hides all bars in that
      group.
  * The regression line on the scatter plot shows confidence interval.
  * Better support for mobile and print.
  * JSON escaping has been made more robust by no longer directly injecting
    reports as JavaScript code.

1.5.7.0

* Warn if an HTML report name contains newlines, and replace newlines with
  whitespace to avoid syntax errors in the report itself.

1.5.6.2

* Use unescaped HTML in the `json.tpl` template.

1.5.6.1

* Bundle `criterion-examples`' `LICENSE` file.

1.5.6.0

* Allow building with `base-compat-batteries-0.11`.

1.5.5.0

* Fix the build on old GHCs with the `embed-data-files` flag.
* Require `transformers-compat-0.6.4` or later.

1.5.4.0

* Add `parserWith`, which allows creating a `criterion` command-line interface
  using a custom `optparse-applicative` `Parser`. This is usefule for sitations
  where one wants to add additional command-line arguments to the default ones
  that `criterion` provides.

  For an example of how to use `parserWith`, refer to
  `examples/ExtensibleCLI.hs`.

* Tweak the way the graph in the HTML overview zooms:

  * Zooming all the way out resets to the default view (instead of continuing
    to zoom out towards empty space).
  * Panning all the way to the right resets to the default view in which zero
    is left-aligned (instead of continuing to pan off the edge of the graph).
  * Panning and zooming only affecs the x-axis, so all results remain in-frame.

1.5.3.0

* Make more functions (e.g., `runMode`) able to print the `µ` character on
  non-UTF-8 encodings.

1.5.2.0

* Fix a bug in which HTML reports would render incorrectly when including
  benchmark names containing apostrophes.

* Only incur a dependency on `fail` on old GHCs.

1.5.1.0

* Add a `MonadFail Criterion` instance.

* Add some documentation in `Criterion.Main` about `criterion-measurement`'s
  new `nfAppIO` and `whnfAppIO` functions, which `criterion` reexports.

1.5.0.0

* Move the measurement functionality of `criterion` into a standalone package,
  `criterion-measurement`. In particular, `cbits/` and `Criterion.Measurement`
  are now in `criterion-measurement`, along with the relevant definitions of
  `Criterion.Types` and `Criterion.Types.Internal` (both of which are now under
  the `Criterion.Measurement.*` namespace).
  Consequently, `criterion` now depends on `criterion-measurement`.

  This will let other libraries (e.g. alternative statistical analysis
  front-ends) to import the measurement functionality alone as a lightweight
  dependency.

* Fix a bug on macOS and Windows where using `runAndAnalyse` and other
  lower-level benchmarking functions would result in an infinite loop.

1.4.1.0

* Use `base-compat-batteries`.

1.4.0.0

* We now do three samples for statistics:

  * `performMinorGC` before the first sample, to ensure it's up to date.
  * Take another sample after the action, without a garbage collection, so we
    can gather legitimate readings on GC-related statistics.
  * Then `performMinorGC` and sample once more, so we can get up-to-date
    readings on other metrics.

  The type of `applyGCStatistics` has changed accordingly. Before, it was:

  ```haskell
     Maybe GCStatistics -- ^ Statistics gathered at the end of a run.
  -> Maybe GCStatistics -- ^ Statistics gathered at the beginning of a run.
  -> Measured -> Measured
  ```

  Now, it is:

  ```haskell
     Maybe GCStatistics -- ^ Statistics gathered at the end of a run, post-GC.
  -> Maybe GCStatistics -- ^ Statistics gathered at the end of a run, pre-GC.
  -> Maybe GCStatistics -- ^ Statistics gathered at the beginning of a run.
  -> Measured -> Measured
  ```

  When diffing `GCStatistics` in `applyGCStatistics`, we carefully choose
  whether to diff against the end stats pre- or post-GC.

* Use `performMinorGC` rather than `performGC` to update garbage collection
  statistics. This improves the benchmark performance of fast functions on large
  objects.

* Fix a bug in the `ToJSON Measured` instance which duplicated the
  mutator CPU seconds where GC CPU seconds should go.

* Fix a bug in sample analysis which incorrectly accounted for overhead
  causing runtime errors and invalid results. Accordingly, the buggy
  `getOverhead` function has been removed.

* Fix a bug in `Measurement.measure` which inflated the reported time taken
  for `perRun` benchmarks.

* Reduce overhead of `nf`, `whnf`, `nfIO`, and `whnfIO` by removing allocation
  from the central loops.

1.3.0.0

* `criterion` was previously reporting the following statistics incorrectly
  on GHC 8.2 and later:

  * `gcStatsBytesAllocated`
  * `gcStatsBytesCopied`
  * `gcStatsGcCpuSeconds`
  * `gcStatsGcWallSeconds`

  This has been fixed.

* The type signature of `runBenchmarkable` has changed from:

  ```haskell
  Benchmarkable -> Int64 -> (a -> a -> a) -> (IO () -> IO a) -> IO a
  ```

  to:

  ```haskell
  Benchmarkable -> Int64 -> (a -> a -> a) -> (Int64 -> IO () -> IO a) -> IO a
  ```

  The extra `Int64` argument represents how many iterations are being timed.

* Remove the deprecated `getGCStats` and `applyGCStats` functions (which have
  been replaced by `getGCStatistics` and `applyGCStatistics`).
* Remove the deprecated `forceGC` field of `Config`, as well as the
  corresponding `--no-gc` command-line option.
* The header in generated JSON output mistakenly used the string `"criterio"`.
  This has been corrected to `"criterion"`.

1.2.6.0

* Add error bars and zoomable navigation to generated HTML report graphs.

  (Note that there have been reports that this feature can be somewhat unruly
  when using macOS and Firefox simultaneously. See
  https://github.com/flot/flot/issues/1554 for more details.)

* Use a predetermined set of cycling colors for benchmark groups in HTML
  reports. This avoids a bug in earlier versions of `criterion` where benchmark
  group colors could be chosen that were almost completely white, which made
  them impossible to distinguish from the background.

1.2.5.0

* Add an `-fembed-data-files` flag. Enabling this option will embed the
  `data-files` from `criterion.cabal` directly into the binary, producing
  a relocatable executable. (This has the downside of increasing the binary
  size significantly, so be warned.)

1.2.4.0

* Fix issue where `--help` would display duplicate options.

1.2.3.0

* Add a `Semigroup` instance for `Outliers`.

* Improve the error messages that are thrown when forcing nonexistent
  benchmark environments.

* Explicitly mark `forceGC` as deprecated. `forceGC` has not had any effect
  for several releases, and it will be removed in the next major `criterion`
  release.

1.2.2.0

* Important bugfix: versions 1.2.0.0 and 1.2.1.0 were incorrectly displaying
  the lower and upper bounds for measured values on HTML reports.

* Have `criterion` emit warnings if suspicious things happen during mustache
  template substitution when creating HTML reports. This can be useful when
  using custom templates with the `--template` flag.

1.2.1.0

* Add `GCStatistics`, `getGCStatistics`, and `applyGCStatistics` to
  `Criterion.Measurement`. These are inteded to replace `GCStats` (which has
  been deprecated in `base` and will be removed in GHC 8.4), as well as
  `getGCStats` and `applyGCStats`, which have also been deprecated and will be
  removed in the next major `criterion` release.

* Add new matchers for the `--match` flag:
  * `--match pattern`, which matches by searching for a given substring in
    benchmark paths.
  * `--match ipattern`, which is like `--match pattern` but case-insensitive.

* Export `Criterion.Main.Options.config`.

* Export `Criterion.toBenchmarkable`, which behaves like the `Benchmarkable`
  constructor did prior to `criterion-1.2.0.0`.

1.2.0.0

* Use `statistics-0.14`.

* Replace the `hastache` dependency with `microstache`.

* Add support for per-run allocation/cleanup of the environment with
  `perRunEnv` and `perRunEnvWithCleanup`,

* Add support for per-batch allocation/cleanup with
  `perBatchEnv` and `perBatchEnvWithCleanup`.

* Add `envWithCleanup`, a variant of `env` with cleanup support.

* Add the `criterion-report` executable, which creates reports from previously
  created JSON files.

1.1.4.0

* Unicode output is now correctly printed on Windows.

* Add Safe Haskell annotations.

* Add `--json` option for writing reports in JSON rather than binary
  format.  Also: various bugfixes related to this.

* Use the `js-jquery` and `js-flot` libraries to substitute in JavaScript code
  into the default HTML report template.

* Use the `code-page` library to ensure that `criterion` prints out Unicode
  characters (like ², which `criterion` uses in reports) in a UTF-8-compatible
  code page on Windows.

* Give an explicit implementation for `get` in the `Binary Regression`
  instance. This should fix sporadic `criterion` failures with older versions
  of `binary`.

* Use `tasty` instead of `test-framework` in the test suites.

* Restore support for 32-bit Intel CPUs.

* Restore build compatibilty with GHC 7.4.

1.1.1.0

* If a benchmark uses `Criterion.env` in a non-lazy way, and you try
  to use `--list` to list benchmark names, you'll now get an
  understandable error message instead of something cryptic.

* We now flush stdout and stderr after printing messages, so that
  output is printed promptly even when piped (e.g. into a pager).

* A new function `runMode` allows custom benchmarking applications to
  run benchmarks with control over the `Mode` used.

* Added support for Linux on non-Intel CPUs.

* This version supports GHC 8.

* The `--only-run` option for benchmarks is renamed to `--iters`.

1.1.0.0

* The dependency on the either package has been dropped in favour of a
  dependency on transformers-compat.  This greatly reduces the number
  of packages criterion depends on.  This shouldn't affect the
  user-visible API.

* The documentation claimed that environments were created only when
  needed, but this wasn't implemented. (gh-76)

* The package now compiles with GHC 7.10.

* On Windows with a non-Unicode code page, printing results used to
  cause a crash.  (gh-55)

1.0.2.0

* Bump lower bound on optparse-applicative to 0.11 to handle yet more
  annoying API churn.

1.0.1.0

* Added a lower bound of 0.10 on the optparse-applicative dependency,
  as there were major API changes between 0.9 and 0.10.
