module Neetcode.ArraysAndHashing.HasSum (hasNum, hasNum', hasNum'', hasNum''') where

import qualified Data.Set as Set

-- Есть ли в двух списках числа, чья сумма равна n
type HasNum = [Int] -> [Int] -> Int -> Bool

hasNum'' :: HasNum
hasNum'' ax bx n = any (\el -> n - el `elem` bx) ax

hasNum''' :: HasNum
hasNum''' ax bx n = or [n - a `elem` bx | a <- ax]

hasNum' :: HasNum
hasNum' ax bx n = any (\el -> (n - el) `Set.member` s) ax
  where
    s = Set.fromList bx

hasNum :: HasNum
hasNum ax bx n = or [(n - a) `Set.member` s | a <- ax]
  where
    s = Set.fromList bx