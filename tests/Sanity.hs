import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain [ bench "fib 10" $ whnf fib 10 ]
