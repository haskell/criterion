
1.1.3.1

* Add Safe Haskell annotations.

1.1.3.0

* Add `--json` option for writing reports in JSON rather than binary
  format.  Also: various bugfixes related to this.

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
