module Main where

import Criterion.Main
import Criterion.Main.Options
import Options.Applicative
import Prelude ()
import Prelude.Compat

data CustomArgs = CustomArgs
  { -- This data type adds two new arguments, listed below.
    customArg1    :: Int
  , customArg2    :: String

    -- The remaining arguments come from criterion itself.
  , criterionArgs :: Mode
  }

customParser :: Parser CustomArgs
customParser = CustomArgs
  <$> option auto
      (  long "custom-arg1"
      <> value 42
      <> metavar "INT"
      <> help "Custom argument 1" )
  <*> strOption
      (  long "custom-arg2"
      <> value "Benchmark name"
      <> metavar "STR"
      <> help "Custom argument 2" )
  <*> parseWith defaultConfig

main :: IO ()
main = do
  args <- execParser $ describeWith customParser
  putStrLn $ "custom-arg1: " ++ show (customArg1 args)
  putStrLn $ "custom-arg2: " ++ customArg2 args
  runMode (criterionArgs args)
    [ bench (customArg2 args) $ whnf id $ customArg1 args ]
