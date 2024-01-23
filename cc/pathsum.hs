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
  let rows = [[9], [5,4], [3,7,7], [6,4,8,6]]
   in print $ pathSum 0 rows
