module Codewars.Kyu6.TakeANumberAndSumItsDigitsaisedTo (sumDigPow) where

{- 6 kyu -}

{-
The number 89 is the first integer with more than one digit that fulfills the property partially
introduced in the title of this kata. What's the use of saying "Eureka"? Because this
sum gives the same number:

Task
We need a function to collect these numbers, that may receive two integers
[a,b] (inclusive) and outputs a list of the sorted numbers in the range that fulfills the property described above.
 -}

import Data.List (unfoldr)

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = [x | x <- [a .. b], x == digPow x]
  where
    digPow = sum . zipWith (flip (^)) [(1 :: Int) ..] . digits
    digits n = reverse (unfoldr step (abs n))
    step 0 = Nothing
    step n = let (x, y) = n `divMod` 10 in Just (x, y)
