module Neetcode.ArraysAndHashing.EncodeAndDecodeStrings (strEncode, strDecode) where

import Data.List (unfoldr)
import Text.Read (readMaybe)

{-
https://neetcode.io/problems/string-encode-and-decode
Design an algorithm to encode a list of strings to a string. The encoded string is then sent over the network and is decoded back to the original list of strings.
 -}
strEncode :: [String] -> String
strEncode = concatMap (\str -> show (length str) <> "#" <> str)

strDecode :: String -> [String]
strDecode = unfoldr go
  where
    go str = do
      len <- readMaybe num
      pure $ splitAt len $ drop 1 rest
      where
        (num, rest) = break (== '#') str