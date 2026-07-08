module Codewars.Kyu5.FirstNonRepeatingCharacter (firstNonRepeatingLetter) where

import Data.Char (toLower)
import qualified Data.Map.Strict as M

{- 5 kyu -}

-- | Returns the first unique letter (case-insensitive), if it exists, from the given string.
firstNonRepeatingLetter :: String -> Maybe Char
firstNonRepeatingLetter str = (str !!) <$> index
  where
    index = M.foldr go Nothing dict
      where
        go (freq, ord) acc
          | freq /= (1 :: Int) = acc
          | otherwise = Just (maybe ord (min ord) acc)
    dict =
      M.fromListWith
        (\(f1, i1) (f2, i2) -> (f1 + f2, min i1 i2))
        [(toLower ch, (1, i)) | (ch, i) <- zip str [0 ..]]