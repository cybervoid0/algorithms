module Lib
    ( someFunc
    ) where

{- import Data.Map (Map)
import qualified Data.Map.Strict as M -}

{- cleanString :: String -> String
cleanString = foldr go ""
  where
    go el acc = case M.lookup el dict of
      Nothing -> M.insert el dict
      Just x -> _
    dict :: Map String String
    dict = M.empty -}

someFunc :: IO ()
someFunc = putStrLn "someFunc"
