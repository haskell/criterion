* The "--only-run" option for benchmarks is renamed to "--iters".

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
