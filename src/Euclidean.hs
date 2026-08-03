module Euclidean (euclidean) where

euclidean :: Integer -> Integer -> Integer
euclidean size1 size2
  | height == 0 = width
  | m == 0 = height
  | otherwise = euclidean m height
  where
    (width, height) =
      let (ab1, ab2) = (abs size1, abs size2)
       in (max ab1 ab2, min ab1 ab2)
    m = width `mod` height
