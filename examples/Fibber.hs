{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main

fib :: Int -> Int
fib n | n < 0     = error "negative!"
      | otherwise = go (fromIntegral n)
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

fact :: Int -> Integer
fact n | n < 0     = error "negative!"
       | otherwise = go (fromIntegral n)
    where go 0 = 1
          go i = i * go (i-1)

fio :: Int -> IO Integer
fio n | n < 0     = error "negative!"
      | otherwise = go (fromIntegral n)
    where go i | i == 0    = return 1
               | otherwise = do
            j <- go (i-1)
            return $! i * j

main = defaultMain [
        bgroup "tiny" [ bench "fib 10" $ whnf fib 10
                      , bench "fib 15" $ whnf fib 15
                      , bench "fib 20" $ whnf fib 20
                      , bench "fib 25" $ whnf fib 25
                      ],
        bgroup "fib" [ bench "fib 10" $ whnf fib 10
                     , bench "fib 35" $ whnf fib 35
                     , bench "fib 37" $ whnf fib 37
                     ],
        bgroup "fact" [ bench "fact 100"  $ whnf fact 100
                      , bench "fact 1000" $ whnf fact 1000
                      , bench "fact 3000" $ whnf fact 3000
                      ],
        bgroup "fio" [ bench "fio 100"  (fio 100)
                     , bench "fio 1000" (fio 1000)
                     , bench "fio 3000" (fio 3000)
                     ]
       ]
