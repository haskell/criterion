# Criterion: robust, reliable performance measurement

This package provides the Criterion module, a Haskell library for
measuring and analysing the performance of Haskell programs.

To get started, read the documentation for the Criterion.Main module,
and take a look at the programs in the examples directory.


# Building and installing

To build and install criterion, just run

    cabal install criterion

On Mac OS X, the Chart library that criterion uses is not available,
so you'll have to build without it.

    cabal install criterion -f-chart
 
This will lose you the ability to generate charts, but you'll still be
able to generate CSV files and import them into your favourite
spreadsheet, or gnuplot, or whatnot.


# Get involved!

Please report bugs via the
[bitbucket issue tracker](http://bitbucket.org/bos/criterion).

Master [Mercurial repository](http://bitbucket.org/bos/criterion):

* `hg clone http://bitbucket.org/bos/criterion`

There's also a [git mirror](http://github.com/bos/criterion):

* `git clone git://github.com/bos/criterion.git`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
