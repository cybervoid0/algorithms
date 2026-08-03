module Codewars.Kyu6.DeciferThis (decipherThis) where

import Data.Bifunctor (first)
import Data.Char (chr, isDigit)

{-
You are given a secret message you need to decipher. Here are the things you need to know to decipher it:

For each word:

the second and the last letter is switched (e.g. Hello becomes Holle)
the first letter is replaced by its character code (e.g. H becomes 72)
there are no special characters used, only letters and spaces
words are separated by a single space
there are no leading or trailing spaces
 -}

decipherThis :: String -> String
decipherThis = unwords . map process . words
  where
    process w = h : swapEnds rest
      where
        (h, rest) = first (chr . read) . span isDigit $ w

swapEnds :: String -> String
swapEnds [] = []
swapEnds (x : xs) = case reverse xs of
  [] -> [x]
  (h : t) -> h : reverse t <> [x]