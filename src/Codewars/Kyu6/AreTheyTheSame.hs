module Codewars.Kyu6.AreTheyTheSame (comp) where

import qualified Data.Map.Strict as M

{- 6 kyu -}

{-
Given two arrays a and b write a function comp(a, b) (orcompSame(a, b)) that checks whether the two arrays have the "same" elements, with the same multiplicities (the multiplicity of a member is the number of times it appears). "Same" means, here, that the elements in b are the elements in a squared, regardless of the order.
 -}

comp :: [Integer] -> [Integer] -> Bool
comp as bs = uniqA == uniqB
  where
    uniqA = makeDict . map (^ (2 :: Int)) $ as
    uniqB = makeDict bs

makeDict :: [Integer] -> M.Map Integer Integer
makeDict = M.fromListWith (+) . map (\x -> (x, 1))
