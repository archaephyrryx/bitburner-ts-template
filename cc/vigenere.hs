module Main where

import Data.Char

alphaPos :: Char -> Int
alphaPos c = ord c - ord 'A'

vig :: String -> String -> String
vig key = zipWith (\a b -> chr $ mod (alphaPos a + alphaPos b) 26 + ord 'A') (cycle key)

main :: IO ()
main = do
  text <- getLine
  let [msg, key] = read text :: [String]
  putStrLn $ vig key msg
