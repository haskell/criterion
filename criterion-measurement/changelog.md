next

* Ensure that `Criterion.Measurement.Types.Internal` is always compiled with
  optimizations, even if the `criterion-measurement` library itself happens to
  be built with `-O0` or `-fprof-auto`. This is necessary to ensure that the
  inner benchmarking loop of criterion always finishes in a timely manner,
  even if the rest of the library is not fully optimized.

0.1.1.0

* Add `nfAppIO` and `whnfAppIO` functions, which take a function and its
  argument separately like `nf`/`whnf`, but whose function returns `IO` like
  `nfIO`/`whnfIO`. This is useful for benchmarking functions in which the bulk
  of the work is not bound by IO, but by pure computations that might otherwise
  be optimized away if the argument is known statically.

0.1.0.0

* This is the first release of `criterion-measurement`. The changelog notes
  below are copied from the notes for the corresponding `criterion` release,
  `criterion-1.5.0.0`.

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
