{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fact :: Integer -> Integer
fact n | n < 0     = error "negative!"
       | otherwise = go n
    where go i | i == 0    = 1
               | otherwise = i * fact (i-1)

main = defaultMain [
        bgroup "fib" [ bench "fib 10" (\(_::Int) -> fib 10)
                     , bench "fib 35" (\(_::Int) -> fib 35)
                     , bench "fib 37" (\(_::Int) -> fib 37)
                     ],
        bgroup "fact" [ bench "fact 100" (\(_::Int) -> fact 100)
                      , bench "fact 350" (\(_::Int) -> fact 350)
                      , bench "fact 700" (\(_::Int) -> fact 700)
                      ]
       ]
