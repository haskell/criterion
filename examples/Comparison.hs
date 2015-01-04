import Criterion.Main

main = defaultMain [
   bench "exp" $ whnf exp (2 :: Double)
 , bench "log" $ whnf log (2 :: Double)
 , bench "sqrt" $ whnf sqrt (2 :: Double)
 ]
