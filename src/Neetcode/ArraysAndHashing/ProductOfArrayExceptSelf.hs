module Neetcode.ArraysAndHashing.ProductOfArrayExceptSelf (productExceptSelf) where

{-
https://neetcode.io/problems/products-of-array-discluding-self/
Given an integer array nums, return an array output where output[i] is the product of all the elements of nums except nums[i].
Each product is guaranteed to fit in a 32-bit integer.
Follow-up: Could you solve it in O(n) time without using the division operation?
 -}
{-
Решение:
нужно собрать частичные произведения справа и слева
 -}
productExceptSelf :: (Num a) => [a] -> [a]
productExceptSelf nums =
  zipWith
    (*)
    (init $ scanl (*) 1 nums)
    (drop 1 $ scanr (*) 1 nums)