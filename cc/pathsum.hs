module Main where

pathSum :: Int -> [[Int]] -> Int
pathSum ix rows = case rows of
  [] -> 0
  row : xt ->
    case drop ix row of
      [x] -> x + pathSum ix xt
      (l : r : _) -> min (l + pathSum ix xt) (r + pathSum (ix + 1) xt)
      _ -> undefined

main :: IO ()
main =
  let rows = [ [7], [9,6], [6,1,9], [4,1,7,8], [6,5,2,2,7] ]
   in print $ pathSum 0 rows
