module Codewars.Kyu7.StalinSort (stalinSort) where

{-
Implement the function stalin_sort / stalinSort, which accepts an array of integers and modifies it in-place, removing all elements that violate the ascending order relative to the previous surviving element.

All other elements are considered enemies of order and must be eliminated.
 -}

stalinSort :: (Ord a) => [a] -> [a]
stalinSort = reverse . go []
  where
    go acc [] = acc
    go [] (el : rest) = go [el] rest
    go acc@(hd : _) (el : rest)
      | el >= hd = go (el : acc) rest -- element added
      | otherwise = go acc rest -- element is shot