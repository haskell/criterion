module Main where

import Criterion
import Criterion.Main

main :: IO ()
main = defaultMain
    [ env (return ()) $
       \ ~() -> bgroup "\"oops\"" [bench "dummy" $ nf id ()]
    , env (return ()) $
       \ ~() -> bgroup "'oops'" [bench "dummy" $ nf id ()]
    ]
