0.2.3.0

* Drop support for pre-8.0 versions of GHC.

0.2.2.0

* Supporting building with all AArch64 platforms (not just Linux and macOS).

0.2.1.0

* Make the behavior of the benchmarking functions independent of the
  `-fspec-const-count` limit.

0.2.0.0

* Add a `measPeakMbAllocated` field to `Measured` for reporting maximum
  megabytes allocated. Naturally, this affects the behavior of `Measured`'s
  `{To,From}JSON` and `Binary` instances.

0.1.4.0

* Fix a bug that occurred with GHC 9.2.4 or later that would cause incorrect
  measurements.

0.1.3.0

* Change `criterion_rdtsc` to return `mach_absolute_time` on macOS. This is a
  portable way of returning the number of CPU cycles that works on both Intel-
  and ARM-based Macs.

* Change `criterion_gettime` to use `clock_gettime_nsec_np` instead of
  `mach_absolute_time` on macOS. While `mach_absolute_time` has nanosecond
  resolution on Intel-based Macs, this is not the case on ARM-based Macs, so
  the previous `mach_absolute_time`-based implementation would return incorrect
  timing results on Apple silicon.

  There are two minor consequences of this change:

  * `criterion-measurement` now only supports macOS 10.02 or later, as that is
    the first version to have `clock_gettime_nsec_np`. As macOS 10.02 was
    released in 2002, this is unlikely to affect users, but please speak up if
    this is a problem for you.

  * As `clock_gettime_nsec_np` does not require any special initialization
    code, `criterion_inittime` is now a no-op on macOS. If you manually invoke
    the `getTime` function in your code, however, it is still important that
    you `initializeTime` beforehand, as this is still required for the Windows
    implementation to work correctly.

0.1.2.0

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
