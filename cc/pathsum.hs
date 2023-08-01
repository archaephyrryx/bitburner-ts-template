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
  let rows = [[5], [6, 9], [7, 1, 6], [5, 5, 1, 6], [5, 9, 5, 1, 1], [7, 4, 9, 1, 5, 1], [7, 7, 2, 2, 7, 7, 8], [4, 3, 3, 9, 8, 5, 8, 1], [5, 8, 2, 1, 4, 8, 4, 3, 4], [4, 8, 5, 2, 2, 6, 2, 3, 1, 5], [8, 4, 6, 8, 1, 8, 5, 6, 2, 8, 7], [2, 9, 9, 3, 1, 2, 3, 4, 2, 2, 6, 7]]
   in print $ pathSum 0 rows
