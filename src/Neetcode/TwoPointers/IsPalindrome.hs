module Neetcode.TwoPointers.IsPalindrome (isPalindrome, isPalindrome', isPalindrome'') where

import Data.Char (isAlpha, toLower)

{-
https://neetcode.io/problems/is-palindrome
Given a string s, return true if it is a palindrome, otherwise return false.
A palindrome is a string that reads the same forward and backward. It is also case-insensitive and ignores all non-alphanumeric characters.
Note: Alphanumeric characters consist of letters (A-Z, a-z) and numbers (0-9).
 -}
isPalindrome :: String -> Bool
isPalindrome str = n == reverse n
  where
    n = map toLower . filter isAlpha $ str

-- через applicative
isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' = (==) <*> reverse

-- Является ли строка палиндромом
-- ручной рекурсивный метод
isPalindrome' :: String -> Bool
isPalindrome' str = case str of
  [] -> True
  [_] -> True
  [h, t] -> h == t
  [h, _, t] -> h == t
  (h : rest) -> h == last rest && isPalindrome (init rest)
