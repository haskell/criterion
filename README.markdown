# Criterion: robust, reliable performance measurement

This package provides the Criterion module, a Haskell library for
measuring and analysing software performance.

To get started, read the documentation for the
[Criterion.Main](http://hackage.haskell.org/packages/archive/criterion/latest/doc/html/Criterion-Main.html)
module, and take a look at the programs in the
[examples](https://github.com/bos/criterion/tree/master/examples)
directory.


# Building and installing

To build and install criterion, just run

    cabal install criterion

On Mac OS X, the Chart library that criterion uses is not available
unless you go through the painful process of installing gtk and cairo,
so you'll probably have to build without it.

    cabal install criterion -f-chart
 
This will lose you the ability to generate charts, but you'll still be
able to generate CSV files and import them into your favourite
spreadsheet, or gnuplot, or whatnot.


# Get involved!

Please report bugs via the
[github issue tracker](https://github.com/bos/criterion).

Master [github repository](https://github.com/bos/criterion):

* `git clone git://github.com/bos/criterion.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/criterion):

* `hg clone https://bitbucket.org/bos/criterion`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
