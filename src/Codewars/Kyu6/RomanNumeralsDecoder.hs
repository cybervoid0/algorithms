module Codewars.Kyu6.RomanNumeralsDecoder (solution) where

{- 6 kyu -}

-- Each Roman digit is added with a positive sign, or subtracted when
-- it is smaller than the digit to its right (the trailing 0 sentinel
-- lets the last digit always count as positive).
solution :: String -> Int
solution str =
  sum
    . zipWith calc romans
    . (++ [0])
    . drop 1
    $ romans
  where
    calc c n
      | c >= n = c
      | otherwise = -c
    romans = map convert str
    convert n = case n of
      'I' -> 1
      'V' -> 5
      'X' -> 10
      'L' -> 50
      'C' -> 100
      'D' -> 500
      'M' -> 1000
      _ -> 0
