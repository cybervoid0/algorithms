module Codewars.Kyu5.BestTravel (chooseBestSum) where

import Data.List (tails)
import Data.List.NonEmpty (nonEmpty)

{-
John and Mary want to travel between a few towns A, B, C ...
Mary has on a sheet of paper a list of distances between these
towns. ls = [50, 55, 57, 58, 60]. John is tired of driving and
he says to Mary that he doesn't want to drive more than
t = 174 miles and he will visit only 3 towns.

Which distances, hence which towns, they will choose so that
the sum of the distances is the biggest possible to please
Mary and John?
 -}

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum distance numTowns towns = maximum <$> nonEmpty [s | x <- combinations numTowns towns, let s = sum x, s <= distance]
  where
    combinations 0 _ = [[]]
    combinations n xs = [y : ys | y : xs' <- tails xs, ys <- combinations (n - 1) xs']
