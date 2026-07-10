module Codewars.Kyu6.AreAllElementsEqual (eqAll, eqAll', eqAll'') where

import Data.Foldable (toList)

{- 6 kyu -}

{-
Create a function eqAll that determines if all elements of a list are equal.
list can be a list of any Eq instance but you need not support any other Foldables; return value is a Bool.
 -}

eqAll :: (Eq a) => [a] -> Bool
eqAll [] = True
eqAll (x : xs) = all (== x) xs

{-
Create a function eqAll that determines if all elements of a list are equal.
list can be any Foldable structure, of any Eq instance, and may be infinite. Return value is a Bool.
 -}

eqAll' :: (Foldable t, Eq a) => t a -> Bool
eqAll' elements = maybe True (\fe -> all (== fe) elements) firstElem
  where
    firstElem = foldr (\el _ -> Just el) Nothing elements

eqAll'' :: (Foldable t, Eq a) => t a -> Bool
eqAll'' xs = case toList xs of
  [] -> True
  (y : ys) -> all (== y) ys