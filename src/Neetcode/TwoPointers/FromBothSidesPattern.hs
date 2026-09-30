module Neetcode.TwoPointers.FromBothSidesPattern (twoSum') where

import Data.Vector (Vector, (!))

{-
На входе - сортированный массив. Нужно найти 2 таких числа, которые в сумме равны target.
Классический пример задачи на 2 pointers .
 -}
twoSum' :: Vector Int -> Int -> Maybe (Int, Int)
twoSum' vec target = go 0 (length vec - 1)
 where
  go left right
    | left >= right = Nothing
    | otherwise = case (l + r) `compare` target of
        LT -> go (left + 1) right
        EQ -> Just (l, r)
        GT -> go left (right - 1)
   where
    l = vec ! left
    r = vec ! right

{-
  Два указателя: L с левого края, R с правого. Массив отсортирован, поэтому:
    сумма < target  ->  L вправо (сумма растёт)
    сумма > target  ->  R влево  (сумма падает)
    сумма = target  ->  нашли

  target = 12

  шаг 1  [ 1 ][ 2 ][ 4 ][ 6 ][ 8 ][ 9 ]
           L                        R     1 + 9 = 10 < 12  ->  L вправо
  шаг 2  [ 1 ][ 2 ][ 4 ][ 6 ][ 8 ][ 9 ]
                L                   R     2 + 9 = 11 < 12  ->  L вправо
  шаг 3  [ 1 ][ 2 ][ 4 ][ 6 ][ 8 ][ 9 ]
                     L              R     4 + 9 = 13 > 12  ->  R влево
  шаг 4  [ 1 ][ 2 ][ 4 ][ 6 ][ 8 ][ 9 ]
                     L         R          4 + 8 = 12 = 12  ->  Just (4, 8)
  Почему ничего не теряем: если arr[L] + arr[R] < target, то arr[L] не даст
  target ни с одним элементом (R уже самый большой из оставшихся), и L можно
  выкинуть. Для R рассуждение симметричное. Итого: <= n шагов, O(n).
-}
