module Codewars.Kyu7.DisemvowelTrolls (disemvowel) where

import Data.Char (toLower)

{- 7 kyu -}

{- Your task is to write a function that takes a string and return a new string with all vowels removed. -}

disemvowel :: String -> String
disemvowel = filter (\ch -> notElem (toLower ch) "aouie")