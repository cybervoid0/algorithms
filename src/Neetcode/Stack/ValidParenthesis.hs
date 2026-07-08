module Neetcode.Stack.ValidParenthesis (isValid) where

{-
https://neetcode.io/problems/validate-parentheses
Valid Parentheses

You are given a string s consisting of the following characters: '(', ')', '{', '}', '[' and ']'.
The input string s is valid if and only if:

- Every open bracket is closed by the same type of close bracket.
- Open brackets are closed in the correct order.
- Every close bracket has a corresponding open bracket of the same type.

Return true if s is a valid string, and false otherwise.
 -}

isValid :: String -> Bool
isValid = go []
  where
    openers = [('(', ')'), ('[', ']'), ('{', '}')]
    closers = map snd openers
    go stack [] = null stack
    go stack (ch : chs)
      -- вот тут важный синтаксис <- это стрелочка matches с таким-то pattern
      | Just closing <- lookup ch openers = go (closing : stack) chs
      | ch `elem` closers = case stack of
          (s : sx) | s == ch -> go sx chs
          _ -> False
      | otherwise = go stack chs