module Main where

import Lib (State (Start), calculate, runState)

main :: IO ()
main =
  let (k, ps) = (5, [89,35,105,144,60,62,156,59,94,125,41,132,190,21,199,38,39,162,134,120,135,40,196,47,158])
   in print $ calculate $ runState (Start k) ps