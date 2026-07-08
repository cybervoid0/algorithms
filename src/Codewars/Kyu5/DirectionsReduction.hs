module Codewars.Kyu5.DirectionsReduction (dirReduce) where

{-
Write a function dirReduc which will take an array of strings and returns
an array of strings with the needless directions removed (W<->E or S<->N side by side).
 -}
data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce = foldr reducer []
  where
    reducer d (e : es)
      | e == opposite d = es
    reducer d es = d : es
    opposite dir = case dir of
      North -> South
      South -> North
      East -> West
      West -> East
