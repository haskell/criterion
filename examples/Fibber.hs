{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fact :: Int -> Integer
fact n | n < 0     = error "negative!"
       | otherwise = go (fromIntegral n)
    where go i | i == 0    = 1
               | otherwise = i * go (i-1)

fio :: Int -> IO Integer
fio n | n < 0     = error "negative!"
      | otherwise = go (fromIntegral n)
    where go i | i == 0    = return 1
               | otherwise = do
            j <- go (i-1)
            return $! i * j

main = defaultMain [
        bgroup "fib" [ bench "fib 10" $ \n -> fib (10+n-n)
                     , bench "fib 35" $ \n -> fib (35+n-n)
                     , bench "fib 37" $ \n -> fib (37+n-n)
                     ],
        bgroup "fact" [ bench "fact 100"  $ \n -> fact (100+n-n)
                      , bench "fact 1000" $ \n -> fact (1000+n-n)
                      , bench "fact 3000" $ \n -> fact (3000+n-n)
                      ],
        bgroup "fio" [ bench "fio 100"  (fio 100)
                     , bench "fio 1000" (fio 1000)
                     , bench "fio 3000" (fio 3000)
                     ]
       ]
