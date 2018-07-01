1.5.0.0

* This is the first release of `criterion-measurement`. The changelog notes
  below are copied from the notes for the corresponding release of
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
