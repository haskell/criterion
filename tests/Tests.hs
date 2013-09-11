module Main (main) where

import Test.Framework (defaultMain)

import Properties

main :: IO ()
main = defaultMain [Properties.tests]
