module Codewars.Kyu6.ReusableMemoization (memo) where

{-
Recursive algorithms can sometimes be optimized with memoisation. Often however,
the memoisation is tightly coupled with the algorithm, making reuse difficult.

Task
Implement a reusable memoisation function
that, given a function of one argument, returns a memoised function of one argument.

Functions of more than one argument can be memoised by currying the function and memoizing
it for every argument, one at a time. This has easier reusability than having a
different memoisation component for every number of arguments.
( This scenario will be tested. )
 -}

memo :: (Enum a) => (a -> b) -> (a -> b)
memo fn = (table !!) . fromEnum
  where
    table = map (fn . toEnum) [0 ..]