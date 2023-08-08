{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (sort)

data Interval = Interval Int Int
  deriving (Eq)

instance Show Interval where
  show :: Interval -> String
  show (Interval s e) = "[" ++ show s ++ ", " ++ show e ++ "]"

instance Ord Interval where
  compare (Interval s e) (Interval s' e') = compare e e' <> compare (e - s) (e' - s')

mergeIntervals :: [(Int, Int)] -> [Interval]
mergeIntervals = mergeIntervals' . map (uncurry Interval)
  where
    mergeIntervals' :: [Interval] -> [Interval]
    mergeIntervals' = go . sort
      where
        go :: [Interval] -> [Interval]
        go [] = []
        go [x] = [x]
        go (x : y : xs)
          | x `overlaps` y = go $ merge x y : xs
          | otherwise = x : go (y : xs)

overlaps :: Interval -> Interval -> Bool
overlaps (Interval s e) (Interval s' e') =
  s <= e' && e >= e'
    || s' <= e && e' >= e

merge :: Interval -> Interval -> Interval
merge (Interval s e) (Interval s' e') =
  Interval (min s s') (max e e')

main :: IO ()
main = do
  line <- getLine
  let intervals = read line :: [[Int]]
  print . mergeIntervals . map (\case [a, b] -> (a, b); _ -> undefined) $ intervals