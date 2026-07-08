module Neetcode.ArraysAndHashing.LongestConsecutiveSequence (longestConsecutive) where

import qualified Data.Set as Set

{-
https://neetcode.io/problems/longest-consecutive-sequence
Given an array of integers nums, return the length of the longest consecutive sequence of elements that can be formed.
A consecutive sequence is a sequence of elements in which each element is exactly 1 greater than the previous element. The elements do not have to be consecutive in the original array.
You must write an algorithm that runs in O(n) time.
 -}
{-
Создаётся сет все элементов списка, затем строится list comprehension из точек начала последовательностей. Из каждой точки восстанавливаем последовательности путём прибавления 1 до тех пор
пока число находится в сете, потом берем длины этих последовательностей и вычисляем максимум
 -}

longestConsecutive :: [Int] -> Int
longestConsecutive nums = maximum [length $ takeWhile (`Set.member` s) [x ..] | x <- nums, Set.notMember (x - 1) s]
  where
    s = Set.fromList nums