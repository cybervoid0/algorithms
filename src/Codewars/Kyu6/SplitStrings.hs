module Codewars.Kyu6.SplitStrings (solution, solution') where

import Data.List.Split (chunksOf)

{- 6 kyu -}

{-
Complete the solution so that it splits the string into strings of two characters in a
list/array (depending on the language you use). If the string contains an odd number of
 characters then it should replace the missing second character of the final pair
with an underscore ('_').
 -}

solution :: String -> [String]
solution = map (take 2 . (++ "_")) . chunksOf 2

solution' :: String -> [String]
solution' [] = []
solution' [x] = [x : "_"]
solution' (x1 : x2 : xs) = [x1, x2] : solution xs