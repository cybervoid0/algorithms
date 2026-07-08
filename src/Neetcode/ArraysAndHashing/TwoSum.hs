module Neetcode.ArraysAndHashing.TwoSum (twoSum) where

import qualified Data.Map.Strict as Map

{-
https://neetcode.io/problems/two-integer-sum/question
Given an array of integers nums and an integer target, return the indices i and j such that nums[i] + nums[j] == target and i != j.
You may assume that every input has exactly one pair of indices i and j that satisfy the condition.
Return the answer with the smaller index first.
-}

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum nums target = go (zip nums [0 ..]) Map.empty
  where
    go [] _ = Nothing
    go ((num, lastInd) : rest) seen = case Map.lookup (target - num) seen of
      Just firstInd -> pure (firstInd, lastInd)
      Nothing -> go rest $ Map.insert num lastInd seen
