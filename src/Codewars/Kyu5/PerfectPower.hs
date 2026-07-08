module Codewars.Kyu5.PerfectPower (isPP) where

import Data.Maybe (listToMaybe)

{-
A perfect power is a classification of positive integers:

In mathematics, a perfect power is a positive integer that can be expressed as an integer power of another positive integer. More formally, n is a perfect power if there exist natural numbers m > 1, and k > 1 such that mk = n.

Your task is to check whether a given integer is a perfect power. If it is a perfect power, return a pair m and k with mk = n as a proof. Otherwise return Nothing, Nil, nil, null, NULL, None or your language's equivalent.

Note: For a perfect power, there might be several pairs. For example 81 = 3^4 = 9^2, so (3, 4) and (9, 2) are both valid solutions. However, the tests take care of this, so if a number is a perfect power, return any pair that proves it.
 -}

isPP :: Integer -> Maybe (Integer, Integer)
isPP n = listToMaybe [(m, k) | k <- takeWhile (\x -> 2 ^ x <= n) [2 ..], let m = iroot k, m ^ k == n]
  where
    -- ⌊n^(1/k)⌋ это библиотечная функция по вычислению корня k-ой степени из n
    iroot k = go 1 (head [h | h <- iterate (* 2) 1, h ^ k >= n])
      where
        go lo hi
          | lo >= hi = lo
          | mid ^ k <= n = go mid hi
          | otherwise = go lo (mid - 1)
          where
            mid = (lo + hi + 1) `div` 2