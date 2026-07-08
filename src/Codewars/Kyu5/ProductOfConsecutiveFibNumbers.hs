module Codewars.Kyu5.ProductOfConsecutiveFibNumbers (productFib) where

import Control.Monad (guard)
import Data.Maybe (fromMaybe, listToMaybe)

{- 5 kyu -}

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib n = fromMaybe (0, 0, False) . listToMaybe $ do
  (a, b) <- zip fib (drop 1 fib)
  guard (a * b >= n)
  pure (a, b, a * b == n)
  where
    fib = 0 : 1 : zipWith (+) fib (drop 1 fib)
