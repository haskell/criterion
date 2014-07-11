import Criterion.Main (bench, defaultMain, whnf)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
  let tooLong = 30
  wat <- timeout (tooLong * 1000000) $
         defaultMain [ bench "fib 10" $ whnf fib 10 ]
  case wat of
    Just () -> return ()
    Nothing -> do
      hPutStrLn stderr $ "*** killed for running longer than " ++
                         show tooLong ++ " seconds!"
      exitFailure
