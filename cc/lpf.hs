{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)

isPrime n = n > 1 &&
    foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
          True primes

primeFactors n | n > 1 = go n primes   -- or go n (2:[3,5..])
   where                               -- for one-off invocation
     go n ps@(p:t)
        | p*p > n    = [n]
        | r == 0     =  p : go q ps
        | otherwise  =      go n t
                where
                  (q,r) = quotRem n p

primes = 2 : filter isPrime [3,5..]

lpf :: Integer -> Integer
lpf = cond isPrime id $ last . primeFactors

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p t f x = if p x then t x else f x

main :: IO ()
main = do
  n <- getArgs >>= \case
    [] -> read <$> getLine
    n : _ -> pure $ read n
  putStrLn $ show $ lpf n

