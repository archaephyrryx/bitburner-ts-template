module Main where

import Lib (State (Start), calculate, runState)

main :: IO ()
main =
  let (k, ps) = (1, [52, 69, 138, 88, 172, 42, 111, 65, 28, 24, 60, 88, 95, 90, 1, 126])
   in print $ calculate $ runState (Start k) ps