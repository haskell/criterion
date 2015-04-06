module Main (main) where

import Options

main :: IO ()
main = do
  cmd <- parseCommandLine
  case cmd of
    Version -> putStrLn versionInfo
    _ -> print cmd
