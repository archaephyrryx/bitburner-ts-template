module Main where

c :: Int -> Int -> Int
c n k
  | k > n = error "cannot choose nCk for k > n"
  | otherwise = (product [(n-k+1) .. n]) `div` (product [1..k])

nPaths :: Int -> Int -> Int
nPaths m n =
  let moves = (m - 1) + (n - 1)
   in moves `c` (min (n - 1) (m - 1))


-- Compute the number of unique paths from the upper-left corner of an NxM lattice to the bottom-right, where the only moves are Down and Right
main :: IO ()
main = do
  text <- getLine
  let [m, n] = read text :: [Int]
  putStrLn $ show $ nPaths m n
