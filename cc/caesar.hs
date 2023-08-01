{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)

lshift :: Int -> Char -> Char
lshift n x
  | n `mod` 26 == 0 = x
  | otherwise =
      case x of
        'A' -> lshift (n - 1) 'Z'
        ' ' -> ' '
        _ -> lshift (n - 1) (pred x)

main :: IO ()
main = do
  getArgs >>= \case
    n : _ -> do
      line <- getLine
      putStrLn $ map (lshift (read n)) line
    _ -> error "Invalid arguments"