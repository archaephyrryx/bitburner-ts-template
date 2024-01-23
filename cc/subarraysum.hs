module Main (main) where

import Control.Monad.Trans.State.Lazy (State, get, put, runState)
import Data.List (scanl, scanr)
import Data.Map (Map)
import Data.Map qualified as M

bestSum :: [Int] -> Int
bestSum xs = go (minimum xs - 1) 0 xs
  where
    go m acc [] = m
    go m acc (x : xs) =
      let acc' = max x (acc + x)
          m' = max m acc'
       in go m' acc' xs

main :: IO ()
main = do
  line <- getLine
  let xs = read ('[' : line ++ "]") :: [Int]
  print $ bestSum xs