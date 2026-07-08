module Codewars.Kyu5.HumanReadableTime (humanReadable) where

import Text.Printf (printf)

{- 5 kyu -}

{- Write a function, which takes a non-negative integer (seconds) as input and returns the time in a human-readable format (HH:MM:SS) -}

humanReadable :: Int -> String
humanReadable sec = printf "%02d:%02d:%02d" h m s
  where
    (h, r) = sec `divMod` 3600
    (m, s) = r `divMod` 60
