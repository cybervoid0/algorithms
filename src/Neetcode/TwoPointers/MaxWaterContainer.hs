module Neetcode.TwoPointers.MaxWaterContainer (maxArea) where

import Data.Array (listArray, (!))

{-
https://neetcode.io/problems/max-water-container/question
You are given an integer array heights where heights[i] represents the height of the
i-th bar.
You may choose any two bars to form a container. Return the maximum amount of water a container can store.
 -}

maxArea :: [Int] -> Int
maxArea hs = go 0 (length hs - 1) 0
  where
    arr = listArray (0, length hs - 1) hs
    go l r mx
      | l >= r = mx
      | lh <= rh = go (l + 1) r newMx
      | otherwise = go l (r - 1) newMx
      where
        lh = arr ! l
        rh = arr ! r
        square = (r - l) * min lh rh
        newMx = max mx square
