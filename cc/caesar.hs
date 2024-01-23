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

rshift :: Int -> Char -> Char
rshift n x
  | n `mod` 26 == 0 = x
  | otherwise =
    case x of
      'Z' -> rshift (n - 1) 'A'
      ' ' -> ' '
      _ -> rshift (n - 1) (succ x)



cShift :: Int -> String -> String
cShift 0 = id
cShift n
  | n < 0 = map $ lshift (abs n)
  | otherwise = map $ rshift n

main :: IO ()
main = do
  getArgs >>= \case
    n : _ -> do
      line <- getLine
      putStrLn $ cShift (read n) line
    _ -> error "Invalid arguments"
