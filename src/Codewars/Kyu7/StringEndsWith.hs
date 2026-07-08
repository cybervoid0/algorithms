module Codewars.Kyu7.StringEndsWith (solution) where

import Data.List (isSuffixOf)

{- 7 kyu -}

{- Complete the solution so that it returns true if the first argument(string) passed in ends with the 2nd argument (also a string). -}

solution :: String -> String -> Bool
solution = flip isSuffixOf
