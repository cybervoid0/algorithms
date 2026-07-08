module Codewars.Kyu6.AreAllElementsEqual (eqAll) where

{- 6 kyu -}

{-
Create a function eqAll that determines if all elements of a list are equal.
list can be a list of any Eq instance but you need not support any other Foldables; return value is a Bool.
 -}

eqAll :: (Eq a) => [a] -> Bool
eqAll [] = True
eqAll (x : xs) = all (== x) xs