module Codewars.Kyu8.RemoveFirstAndLastCharacter (removeChar) where

{- 8 kyu -}

{-
Your goal is to write a function that removes the first and last characters of a string. You're given one parameter, the original string.

Important: Your function should handle strings of any length ≥ 2 characters. For strings with exactly 2 characters, return an empty string.
 -}

removeChar :: String -> String
removeChar (_ : xs) = (init xs)
removeChar _ = ""
