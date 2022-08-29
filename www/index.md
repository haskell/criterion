% criterion: a Haskell microbenchmarking library
% Benchmarking made easy for Haskell.
  <a href="tutorial.html" class="btn btn-primary btn-lg" role="button">Tutorial</a> <a href="report.html" target="_blank" class="btn btn-info btn-lg" role="button">Example</a>



`criterion` is a library that makes accurate microbenchmarking in
Haskell easy.

<a href="fibber.html" target="_blank"><img src="fibber-screenshot.png"></img></a>


# Features

* The simple API hides a lot of automation and details that you
  shouldn't need to worry about.

* Sophisticated, high-resolution analysis can accurately measure
  operations that run in as little as a few hundred picoseconds.

* [Output to active HTML](report.html) (with Javascript charts), CSV,
  and JSON.  Write your own report templates to customize exactly how
  your results are presented.

* Linear regression model allows measurement of the effects of garbage
  collection and other factors.

* Measurements are cross-validated to ensure that sources of
  significant noise (usually other activity on the system) can be
  identified.


# A complete example

This is a complete program that defines a group of three benchmarks.

~~~~ {.haskell}
import Criterion.Main

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]
~~~~
([examples/Fibber.hs](https://github.com/haskell/criterion/blob/master/examples/Fibber.hs))


<div class="jumbotron" style="margin-top: 40px;">
<h2 style="margin-top: 20px;">Ready to jump in?</h2>

I've worked hard to make `criterion` easy to learn, so that you can
write dependable, accurate benchmarks without having to become a
benchmarking expert.

<a href="tutorial.html" class="btn btn-success btn-lg" role="button">Tutorial</a>

I'm proud of the example-filled docs.

<a href="http://hackage.haskell.org/package/criterion" class="btn btn-info btn-lg" role="button">Documentation</a>

If you run into problems, let me know.

<a href="https://github.com/haskell/criterion" class="btn btn-warning btn-lg" role="button">Issues</a>

</div>
