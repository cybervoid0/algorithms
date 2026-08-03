module Codewars.Kyu6.SieveOfEratosphenes (primes, primes') where

import Data.List ((\\))

-- Create a program that is able to calculate all the prime numbers up to a higher bound.

primes :: Int -> [Int]
primes n = sieve [2 .. n]

-- | Sieve of Erathosphenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primes' :: Int -> [Int]
primes' n = sieve' [2 .. n]
  where
    sieve' (x : xs) = x : sieve (xs \\ [x, x + x .. n])
    sieve' [] = []
