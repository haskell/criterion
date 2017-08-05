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
