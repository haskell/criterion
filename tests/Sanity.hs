import Criterion.Main (bench, defaultMain, whnf)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

fib :: Int -> Int
fib = sum . go
  where go 0 = [0]
        go 1 = [1]
        go n = go (n-1) ++ go (n-2)

main :: IO ()
main = do
  let tooLong = 30
  wat <- timeout (tooLong * 1000000) $
         defaultMain [ bench "fib 10" $ whnf fib 10
                     , bench "fib 22" $ whnf fib 22 ]
  case wat of
    Just () -> return ()
    Nothing -> do
      hPutStrLn stderr $ "*** killed for running longer than " ++
                         show tooLong ++ " seconds!"
      exitFailure
