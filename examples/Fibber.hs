{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main

fib :: Int -> Integer
fib m | m < 0     = error "negative!"
      | otherwise = go (fromIntegral m :: Integer)
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

fact :: Int -> Integer
fact n | n < 0     = error "negative!"
       | otherwise = go (fromIntegral n)
    where go 0 = 1
          go i = i * go (i-1)

factIO :: Int -> IO Integer
factIO n | n < 0     = error "negative!"
         | otherwise = go (fromIntegral n)
    where go i | i == 0    = return 1
               | otherwise = do
            j <- go (i-1)
            return $! i * j

main :: IO ()
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
        bgroup "fact" [
          bgroup "pure" [ bench "100"  $ whnf fact 100
                        , bench "1000" $ whnf fact 1000
                        , bench "3000" $ whnf fact 3000
                        ],
          bgroup "IO" [ bench "100"  $ whnfIO (factIO 100)
                      , bench "1000" $ whnfIO (factIO 1000)
                      , bench "3000" $ whnfIO (factIO 3000)
                      ]
        ]
       ]
