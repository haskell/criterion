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
