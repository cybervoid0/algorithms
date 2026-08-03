module Codewars.Kyu6.PascalTriangle (pascalsTriangle) where

pascalsTriangle :: Int -> [Int]
pascalsTriangle = concat . rows
  where
    rows n = take n . iterate (\l -> zipWith (+) (l <> [0]) (0 : l)) $ [1]
