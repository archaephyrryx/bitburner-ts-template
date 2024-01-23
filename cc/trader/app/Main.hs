module Main where

import Lib (State (Start), calculate, runState)

main :: IO ()
main =
  let (k, ps) = (2, [33,140,138,62,47,67,137,21,176,177,175,199,49,86,40,29,76,185])
   in print $ calculate $ runState (Start k) ps
