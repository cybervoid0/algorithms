module Codewars.Kyu6.GuessTheNumberMonad (guess, maybeNum) where

{-
Your task is to guess a number between 1 and 100 using only 7 tests.

You are given a function greaterThan :: Monad m => Int -> m Bool.
 -}

guess :: (Monad m) => (Int -> m Bool) -> m Int
guess gt = go 1 100
  where
    go start end
      | start == end = pure start
      | otherwise = do
          isGreater <- gt mid
          if isGreater
            then go (mid + 1) end
            else go start mid
      where
        mid = (start + end) `div` 2

-- пример использования

guessMaybe :: Int -> Maybe Bool
guessMaybe num = Just (98 > num)

maybeNum :: Maybe Int
maybeNum = guess guessMaybe