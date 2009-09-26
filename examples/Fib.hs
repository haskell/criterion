{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = defaultMain [
         bench "fib 10" (\(_::Int) -> fib 10)
       , bench "fib 29" (\(_::Int) -> fib 35)
       , bench "fib 31" (\(_::Int) -> fib 37)
       ]
